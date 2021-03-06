Tests design notes
==================

These notes describe the design of the test generation software for
the CCI API.

Generated tests are intended to exercise CCI features as thoroughly as
possible. Because there is a big space of possible interactions
between processes communicating through CCI, instead of writing
a fixed set of tests, we generate randomly some programs producing
interactions in this space.

Sometimes the programs will fail to complete, and sometimes the output
produced by them won't be the expected. In both situations the
programs should be reported.

We describe the interactions to test with a small language embedded in
Haskell. This interactions are then executed with an interpreter which
uses the CCI Haskell bindings. As we are interested in having the bugs
fixed soon after they are discovered, we provide a means to translate
our failing tests to C. We have created a thin layer over CCI in C,
which mimics the primitives of our Haskell EDSL. Having a C program
which reproduces a bug, makes it easier to debug and saves CCI
developers the need to be familiar with Haskell.

Property testing
----------------

At the top level we provide a function similar to the following:

    testProp :: (TestError -> IO ())
             -> ([ProcCommand] -> [[Response]] -> [(String,Bool)])
             -> IO [TestError]

The function `testProp f g` generates many interactions and executes
them. For each interaction that completes, it pass to function `g` the
description of the interaction (`[ProcCommand]`) and the output of each
participating process (`[[Responses]]`). Function `g` then yields a
list of conditions (each being a name and a boolean) that either hold
or not. For every interaction that doesn't complete or doesn't satisfy
all conditions of `g`, the error reporting function `f` is called.
Finally, the list of all found errors is returned.

    type TestError = (String,[ProcCommand],[[Response]])

The test error carries a textual description of the error, an
interaction and the output of it. In a fashion imitating QuickCheck,
when a failing interaction is found, it is shrinked and it is the
shrinking result the one reported.

The interaction EDSL
--------------------

An interaction is a sequence of commands for various processes.

    type ProcCommand = ([Int],Command)

A process command holds the identifiers of the processes (`[Int]`) to
which a command is directed. Instead of using a representation as
`[ProcCommand]` for interactions, we could have chosen
`[(Int,Command)]` and have several commands in the latter for each
command involving several processes in the former. This decision has an
impact later when generating and interpreting interactions. The first
representation turned out the most practical.

Here's a listing of the possible commands:
```
data Command =
    ConnectTo String Int WordPtr (Maybe Word64) -- ^ @ConnectTo uri process_id connection_id timeout@ process id is
                                                -- only used on the test driver side
  | Send WordPtr WordPtr Msg                    -- ^ @Send connection_id send_id msg@ message
  | Accept WordPtr                              -- ^ @Accept connection_id@
  | Reject WordPtr                              -- ^ @Reject connection_id@
  | Disconnect WordPtr                          -- ^ @Disconnect connection_id@
  | WaitConnection WordPtr                      -- ^ Wait for a given connection to be established.
  | WaitSendCompletion WordPtr WordPtr          -- ^ @WaitSendCompletion conn_id send_id@ Wait for a send completion to arrive.
  | WaitRecv WordPtr WordPtr                    -- ^ @WaitRecv conn_id recv_id@ for a receive event to arrive.

  | RMAReuseRMAHandle WordPtr                   -- ^ @RMAReuseRMAHandle conn_id@ marks the connection @conn_id@ to
                                                -- reuse a previous rma handle instead of creating a new one.
  | RMAHandleExchange WordPtr                   -- ^ @RMAHandleExchange conn_id@ exchanges rma handles with the
                                                -- given process through the driver.
  | RMAWaitExchange WordPtr                     -- ^ Wait for an rma exchange to complete on the given connection.
  | RMAWrite WordPtr WordPtr                    -- ^ @RMAWrite conn_id msg_id@
  | RMAPrepareRead WordPtr WordPtr              -- ^ @RMAPrepareRead conn_id msg_id@ writes the rma buffer with predefined data for an rma operation.
  | RMARead WordPtr WordPtr                     -- ^ @RMARead conn_id msg_id@
  | RMAWaitWrite WordPtr WordPtr                -- ^ @RMAWaitWrite conn_id msg_id@
  | RMAWaitRead WordPtr WordPtr                 -- ^ @RMAWaitRead conn_id msg_id@
  | RMAFreeHandles WordPtr                      -- ^ @RMAFreeHandles conn_id@ releases the handles of a connection.

  | Quit
```

### The commands

#### `Accept connection_id`

This command instructs a process to accept a connection bound to the
given identifier connection_id if it ever receives such a connection
request. This operation completes immediately.

#### `Reject connection_id`

Instructs a process to reject a connection bound to the given
identifier `connection_id` if it ever receives such a connection
request. This operation completes immediately.

#### `ConnectTo uri pid connection_id (Just timeout)`

Instructs a process to issue a `cci_connect` call using the given uri
and timeout, and binding the resulting connection with the identifier
`connection_id`. The process completes the command when the
`cci_connect` call returns. pid is the indentifier of the process
listenning at the provided uri. The target process doesn't use it for
anything, but when generating the interaction description the pid is
specified first and later the uri is filled in, which is obtained only
after the interacting processes have been launched.

#### `WaitConnection connection_id`

Instructs a process to poll repeteadly for events using
`cci_get_event` until the connection bound to the identifier
`connection_id` is stablished. The process completes the operation
when `cci_get_event` yields a `CCI_EVENT_CONNECT` or
`CCI_EVENT_ACCEPT` event for the connection.

This operation requires the remote process to be continually polling
for events. This is so because the CCI API uses `cci_get_event` both
to process internal events and to yield events to the user. As of the
date of this writing the verbs driver of CCI does not use a progress
thread. Therefore, this command requires cooperation of the remote
process to complete.

#### Disconnect connection_id

Instructs a process to disconnect from the connection bound to the
identifier `connection_id`. This operation completes when the call to
`cci_disconnect` returns.

#### Send connection_id msg_id msg

Instructs a process to issue a `cci_send` call using the given
connection and message msg, and binding the message to the identifier
`msg_id`. The process completes the command when the `cci_send` call
returns.

#### WaitSendCompletion connection_id msg_id

Instructs a process to poll repeteadly for events using
`cci_get_event` until the message bound to the identifier `msg_id` is
sent. The process completes the operation when `cci_get_event` yields
a `CCI_EVENT_SEND` event for the connection bound to identifier
`connection_id` and the message bound to `msg_id`.

#### WaitRecv conn_id msg_id

Instructs a process to poll repeteadly for events using
`cci_get_event` until the message bound to the identifier `msg_id` is
received. The process completes the operation when `cci_get_event`
yields a `CCI_EVENT_RECV` event for the connection bound to identifier
`connection_id` and the message bound to `msg_id`.

#### RMAReuseRMAHandle connection_id

Instructs a process to reuse a previously allocated buffer if an RMA
handle exchange is ever requested. This operation completes
immediately. If no previously allocated buffer exist a new buffer is
created when the RMA handle exchange is requested.

#### RMAHandleExchange connection_id send_id

Instructs a process to exchange RMA handles with another process
through the connection bound to the identifier `connection_id`. The
process either allocates and registers a new buffer or recycles an
existing one. The choice depends on whether a `RMAReuseRMAHandle`
command was previously issued. This command completes when the call to
`cci_send` for sending the local RMA handle returns.

The `send_id` should be used as the context argument of `cci_send`. It
is guaranteed to don't clash with the contexts of other messages in
the connection.

#### RMAWaitExchange connection_id

Instructs a process to wait for the RMA remote handle on the
connection bound to identifier `connection_id`. This command completes
when both a `CCI_EVENT_RECV` event with the remote handle is yielded
by `cci_get_event` and when the `CCI_EVENT_SEND` event with the
`send_id` provided by `RMAHandleExchange` is yielded as well.

This operation requires cooperation from the other process in order to
complete. The other process needs to be polling with `cci_get_event`.

#### RMAFreeHandles connection_id

Instructs a process to set aside RMA handles that won't be used again
in a given interaction for the connection bound to the identifier
`connection_id`. The handles can be taken back by an `RMAReuseRMAHandle`
command. This command completes immediately.

#### RMAWrite connection_id msg_id

Instructs a process to perform an RMA write operation on the
connection bound to the identifier `connection_id`. The contents of
the RMA transfer are generated from the message identifier `msg_id`.
This command completes when the call to `cci_rma` returns.

#### RMAWaitWrite connection_id msg_id

Instructs a process to wait for an RMA write operation with identifier
`msg_id` on the connection bound to identifier `connection_id` to
complete. This command completes when a `CCI_EVENT_SEND` or
`CCI_EVENT_RECV` event is yielded by `cci_get_event` with the
identifier `msg_id`.

This operation requires cooperation from the other process in order to
complete. The other process needs to be polling with `cci_get_event`.

#### RMAPrepareRead connection_id msg_id

Instructs a process to initialize the RMA buffer registered with the
connection bound to identifier `connection_id` with data generated
from the identifier `msg_id`. The operation completes immediately.

#### RMARead connection_id msg_id

Instructs a process to perform an RMA read operation on the connection
bound to the identifier `connection_id`. The contents of the RMA
transfer should have been produced with a previous `RMAPrepareRead`
command in the remote process. This command completes when the call to
`cci_rma` returns.

#### RMAWaitRead connection_id msg_id

Instructs a process to wait for an RMA read operation with identifier
`msg_id` on the connection bound to identifier `connection_id` to
complete. This command completes when a `CCI_EVENT_SEND` or
`CCI_EVENT_RECV` event is yielded by `cci_get_event` with the
identifier `msg_id`.

This operation requires cooperation from the other process in order to
complete. The other process needs to be polling with `cci_get_event`.

#### Quit

Instructs a process to stop processing commands and terminate. This
operation completes immediately.

### Interpreting commands

We have implemented a program called Worker which interprets
interaction commands. An interaction is started by launching as many
processes as the interaction requires, each process running a copy of
the Worker program. Then commands are fed one by one to each process.
We call the driver process to the process which send commands, thus
orchestrating the interaction.

We want the execution to be as deterministic as possible, so an error
can be easily reproduced. Because of this, we request that each
process indicate completion of a given command before sending the next
command to its respective process.

Since there are some commands which are not possible to complete
without cooperation from the other processes (like `WaitConnection`),
waiting for a command to complete before delivering other commands
could cause a deadlock. Because of this, commands which require
cooperation are send simultaneously to their respective processes
instead of seding them one by one. When all processes have confirmed
completion, execution proceeds to the next bunch of commands.

So far the above has been simpler to achieve when representing
interactions as `[ProcCommand]`. When different actions are required
from different processes simultaneously, a single command is targeted
to multiple processes `([p0...pn],command)` and the worker is
instructed to wait for any of the actions that a process would be
expected to do during the cooperation. A more direct approach would be
to use `[[(Int,Command)]]` as representation for interactions, where
each bunch of commands is explicitly stated.

When a command needs coperation some determinism in the execution is
lost when delivering the command simultaneously to different
processes. We could change the `Worker` so it permanently polls for
events, thus no command would need cooperation explicitly coordinated.
All the `Wait`-like commands could not possibly block, therefore
interpreting commands would be simpler but we would lose some of the
determinism we strive to achieve.

When an interaction is to be executed, it is scanned to find the
amount of needed processes. Then, as many processes running the
`Worker` program are launched. Finally, commands are passed to the
processes. If any process exceeds a timeout of a few seconds to
respond, or if it terminates early, the interaction is reported as
failing.

### Generation and shrinking

The generation of an interaction starts with elemental interactions
which are later merged. The elemental interactions involve
communication between two endpoints. Many of these interactions are
generated for different pairs of processes and then they are merged
randomly.

Currently an elemental interaction opens a connection, sends some
messages, perhaps does some RMA operations and then closes the
connection.

    type Interaction = [(Int,ProcCommand)]

The type `Interaction` pairs each command with an identifier of the
elemental interaction in which the command first appears. All commands
in an elemental interaction get the same identifier bound to them. As
commands are merged into more complex interactions, the identifier is
carried along.

The elemental interaction identifiers allow to strip from a composite
interaction one of its elemental interactions. This is an important
operation for reducing failing interactions to a minimal form which is
easier to debug (so called shrinking).

We also implement another form of shrinking which only removes some
specific commands from an interaction rather than the whole of it.

Since interactions can be arbitrarily large, we constrain the size of
the generated interactions with configuration parameters. These
configuration parameters can be used to limit the total amount of
messages in each elemental interaction, the amount of processes that
interactions can use, the size of the messages or the amount of
simultaneous elemental interactions between a single pair of
processes.

TODO
----

* Change the representation of traces to enable delivering different
  commands to different processes simultaneously.
* Test RMA unregister.
* Test sending many messages.
* Test using a blocking endpoint.
* Improve shrinking by removing send further.
* Implement an infinite test mode in which tests are progressively
  grown.
