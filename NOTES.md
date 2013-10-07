Introduction
============

These notes document some decisions taken during elaboration of the
CCI bindings for Haskell.

The bindings are intended to be as slim as possible, leaving further
features to be built on top if desired. For the sake of usability, the
bindings include some small convenience functions which are not
strictly necessary.

In what follows we discuss the parts of the API.

Initialization & cleanup
========================

The call `initCCI` gathers configuration parameters from the
environment and sets up the internal state. The call `finalizeCCI`
frees this state.

As we will do with many other actions for requesting and releasing
resources, we offer a function `withCCI` which puts an action between
evaluations of `initCCI` and `finalizeCCI` and makes sure that
`finalizeCCI` is eventually called.

Almost all API calls are invalid after `finalizeCCI` has been
evaluated. The most likely behavior is the application crashing if
`finalizeCCI` is evaluated prematurely. The CCI transport
implementations, or the bindings or a layer on top could guard each
API call with a test to see if `finalizeCCI` has been evaluated and if
so produce a more graceful error. We leave this problem to the layers
above and below.

Devices
=======

Devices are interfaces to the network hardware. There is a call
`getDevices` that returns the current state of all devices known to
the CCI transport implementation.

The method `cci_get_devices` of the C API is peculiar in that it returns
memory maintained by the CCI transport implementation. This memory is
never released, but it contents may be updated whenever a later call
to `cci_get_devices` is made. Because of this potential memory update,
we copy all the data into Haskell whenever `getDevices` is evaluated
and provide pure functions for accessing it. This makes evident that
a later call to `getDevices` is necessary if the user wants the device
data updated. Copying the data into Haskell in this case looks
harmless since the amount of devices is limited.

Endpoints
=========

An endpoint is a software interface to a device. It provides the
buffers, queues and other resources required for communication.
Connections to many other processes can be done through a single
endpoint.

There are two functions for creating endpoints:
`createPollingEndpoint` and `createBlockingEndpoint`. The C API
provides a single call `cci_create_endpoint` which should be given
different parameters according to whether the user intends to later
block or poll for events.

In theory, `createBlockingEndpoint` should suffice for both polling
and blocking. But the truth is that different code is used in both
cases, and therefore different bugs have appeared in practice, and it
is likely that different transports offer distinct performance
characteristics with one way or the other. Therefore the need to treat
the two cases separatedly.

There is a function `destroyEndpoint` which releases endpoint
resources. And we include functions `withPollingEndpoint` and
`withBlockingEndpoint` that ensure an endpoint is eventually released.

Listening for events on a destroyed endpoint is likely to cause the
application to crash. Again, we defer producing more graceful errors
to other layers.

Connections
===========

Connections are the context in which communication with a specific
peer occurs. They can have associated reliability and ordering
requirements.

Stablishing a connection requires the peers to engage in client and
server roles. The client peer issues a call `connect` specifying
a remote endpoint, and the server peer must be listening for events on
that endpoint. The client peer then needs to listen for events to
learn if its connection request was accepted or not. When receiving
a connection request, the server can use calls `accept` or `reject`
for that purpose.

After the connection is established, either of the peers can issue
a call to `disconnect` to terminate the connection.

Since connections are object whose attributes do not change during its
lifetime, we provide pure accessor functions. The connection offers
however a `context` field which is used to handle an pointer provided
by the user. It may be possible that we make this field writable in
the future. We represent this user suplied pointers in Haskell with
the `Foreign.Ptr.WordPtr` type.

As the number of connection attributes
(reliability/ordering/multicast) is small, we translate C enumeration
into a Haskell enumerated type (abstract datatype with an Enum
instance). More efficient solutions are possible though not as
convenient.

Data transfers
==============

The call `send` can be used to deliver a `ByteString` through
a connection. The `ByteString` length is limited by the maximum send
size allowed by the connection.

The C API provides a call `cci_send` which takes flags `BLOCKING`,
`SILENT` and `NO_COPY`. Some of these flags can appear together but not
all combinations make sense. Also, the semantics of `cci_send` change
so much with the variations that we have decided to put them in
separate calls. Some of these calls take the message in the form of
a `ByteString`, and other calls take a `CStringLen`. The `CStringLen`
form is used when using the flag `NO_COPY` with `cci_send`, which
demands the message buffer to be kept alive until the send completion
arrives.

Most send calls use an unsafe foreign call, except those intended to
block, which use a safe foreign call so the rest of the Haskell
runtime threads can continue execution.

RMA operations are done with the calls `rmaRead` and `rmaWrite`.
Unlike in the C API, we use different types for local and remote RMA
handles. These handles are obtained via the functions `rmaRegister` and
`createRMARemoteHandle`.

RMA handles are deregistered with the call `rmaDeregister`, and there
is a call `withRMALocalHandle` that ensures the handle is
deregistered.

The buffers for RMA operations are not wrapped in bytestrings in the
call `rmaRegister`. This is to hint more explicitly that they are
expected to remain valid until deregistered.

Events
======

Events are produced for connection requests, send completions, receive
events, etc. They are pulled out of the transport implementation with
the call getEvent. Events might be bound to network hardware resources
that are employed to avoid copying data multiple times. Therefore, the
CCI transport implementation needs to be notified when the event is no
longer accessed with a call to `returnEvent`.

Functions `tryWithEventData`, `withEventData` and `pollWithEventData`
ensure that `returnEvent` is eventually called. Since the event data
does not survive evaluation of this functions, the `EventData` type is
given a phantom type parameter for encoding the scope in which the
event data can be accessed.
```
-- | Like 'tryWithEventData' but loops polling until events are available.
pollWithEventData :: 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> (forall s. EventData s -> IO a) -- ^ callback when an event is received.
   -> IO a  -- ^ Yields the callback result.
```
The type variable `a` of `pollWithEventData` cannot be instantiated to
anything containing the type variable `s`, so it provides some
protection from using the event after it has been returned. However it
is possible to sidestep this restriction by launching threads from the
callback or using existential types. We do not try to further protect
against these uses.
```
data EventData s =
   ...
  | EvRecv (EventBytes s) Connection
  ...

newtype EventBytes s = EventBytes CStringLen
```
When an `EvRecv bs conn` event is received, arrival of a new message is being
notified. The message is available as a value of type `EventBytes s`. This is
represented as a pointer to the message data which will be invalidated when
the event is returned. We provide the following functions for converting the
`EventBytes` into a `ByteString`:
```
packEventBytes :: EventBytes s -> IO ByteString

unsafePackEventBytes :: EventBytes s -> IO ByteString
```
The first function copies the data, and the second just wraps the
pointer in a `ByteString`. The second method is unsafe because the
produced `ByteString` will be invalid once the event is returned. The
unsafe function provided some speedups in benchmarks though. Therefore
we pass to the user the decision on what to do with the message data.

We could have used the `CStringLen` type instead of `EventBytes s`,
but the latter clearly delineates what we expect the user to do with
it through the limited functions we offer, and it also carries the
scope type parameter which makes accidental access of returned data
less likely.

Options
=======

There are various parameters that can be inquiered or set from the
transport implementation. For instance, timeouts for sends and
connection requests, or an endpoint URI, or the amount of resources
allotted for the endpoint.

Different parameters use different types for their values. The C API
has gotten away with untyped calls:
```
CCI_DECLSPEC int cci_set_opt(void * handle,cci_opt_name_t name, void *val);
CCI_DECLSPEC int cci_get_opt(void * handle,cci_opt_name_t name, void *val);
```
Here handle can be either a connection or an endpoint object, and the
type of the thing pointed by val depends on the value passed as `name`
which identifies the parameter to query or set.

In Haskell we have made access to parameters typed at the expense of
defining querying and setting functions for each available parameter:
```
getEndpt_KeepAliveTimeout :: Endpoint -> IO Word32
setEndpt_KeepAliveTimeout :: Endpoint -> Word32 -> IO ()

getEndpt_SendTimeout :: Endpoint -> IO Word32
setEndpt_SendTimeout :: Endpoint -> Word32 -> IO ()

getEndpt_SendBufCount :: Endpoint -> IO Word32
setEndpt_SendBufCount :: Endpoint -> Word32 -> IO ()

getEndpt_RecvBufCount :: Endpoint -> IO Word32
setEndpt_RecvBufCount :: Endpoint -> Word32 -> IO ()

getEndpt_RMAAlign :: Endpoint -> IO RMAAlignments

getEndpt_URI :: Endpoint -> IO String

getConn_SendTimeout :: Connection -> IO Word32
setConn_SendTimeout :: Connection -> Word32 -> IO ()
```
Parameters which are read-only do not have setter functions.

Error handling
==============

Most functions in the CCI C API can return an integer error code. We
test for this result on every function and throw an exception if the
call fails. The carries the error code. A function `strError` can be
used to obtain a human readable description of the error.

We currently treat errors as being sparse enough to tolerate being
converted to and from a Haskell enumerated type (abstract data type
with an `Enum` instance).

