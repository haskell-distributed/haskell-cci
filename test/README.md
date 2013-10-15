Tests
=====

Files in this folder implement test generation. This file describes
how to use the test generator. For details on the design of test
generation see [NOTES.md] in the same folder.

Running tests
-------------

The tests are run by issuing the following commands from the top-level
folder:

    cabal configure --enable-tests
    cabal build
    cabal test

If the CCI reference implementation is installed in a non-standard
location then the procedure needs to be changed as follows:

    cabal configure --enable-tests --extra-lib-dirs=/path/to/cci/lib
    cabal build
    LD_LIBRARY_PATH=/path/to/cci/lib cabal test

For testing purposes, the test suite launches several worker
processes. The test suite spoon-feeds each worker with commands
involving CCI calls. Communication with the workers is done through
the standard input and output.

The sequences of commands are randomly produced by the driver. As the
workers perform these commands, they are recorded by the driver in
traces. Whenever a sequence of commands has been fully executed, the
command sequence and the traces produced by each worker are passed to
predicates which evaluate specific conditions on the observed
behavior.

Failing test case generation
----------------------------

Whenever executing a command sequence fails, or a predicate is found
that do not hold for a given trace, the failing case is shrunk by
removing messages and interactions that do not affect the test result.
Then the shrunk command sequence and trace is printed on standard
output, and a C program which reproduces the failing case is stored in
a file t<N>.c where N is a counter increased for each failing case
found.

The generated C program can be compiled and run with:

    gcc -lcci t0.c -I test test/testlib.c -g
    valgrind --suppressions=./cci.supp ./a.out

If CCI is not in a standard location:

    gcc -lcci t0.c -I test test/testlib.c -g -L /path/to/cci/lib -I /path/to/cci/include
    LD_LIBRARY_PATH=/path/to/cci/lib valgrind --suppressions=./cci.supp ./a.out

So far the only meaningful predicate which is implemented is whether
reliable ordered connections deliver packets in an orderly fashion.
But to reach the predicate evaluation stage, workers must have behaved
with some consistency:
* The amount of recv events must match send events.
* The amount of send completion events must match send events.
* The workers should not crash or block waiting for an event which was
  expected.

Some aspects of test generation can be customized via command-line arguments:
```
   --nprocesses[=INT]    Amount of processes to test [default: 2]
   --nsends[=INT]        Amount of messages sent per interaction
                         [default: 4]
   --ntries[=INT]        Amount of command sequences to generate
                         [default: 300]
   --nerrors[=INT]       Amount of errors to collect before stopping
                         testing [default: 2]
   --nminmsglen[=INT]    Minimum size of messages to test [default: 16]
   --nmaxmsglen[=INT]    Maximum size of messages to test [default: 16]
   --nPerProcRuns[=INT]  Amount of interactions each process initiates
                         during a test [default: 2]
   --with-valgrind       Run tests with valgrind [default: False]
```

Editing tests
-------------

Properties over generated traces are written as in `test/test_cci.hs`:
```
main = do
    errs <- testProp defaultTestConfig onError$ \cmds rs ->
              [ ( "sends equal completions"
                , length (filter (isSendCommand . snd) cmds) == length (concatMap (filter isSendCompletion) rs)
                )
              , ( "messages arrive sorted"
                , matchSendRecvs (collectSends cmds) (collectRecvs rs)
                )
              ]
    mapM_ print errs
```
`testProp` takes a test configuration specifying parameters as those
given in the command line to the test suite, and then takes a list of
properties, each property specifies a name and a condition on
a command sequence (`cmds`) and responses (`rs`) (i.e. process
traces). `onError` is a callback evaluated each time an error is
found. It can be useful to print the errors to the standard output as
soone as they are found.

`testProp` generates commands sequences, launches workers and executes
the sequences generating traces or responses from the workers. Then
the properties are evaluated.

A list of errors is returned, which of course will be empty if no
error was found. The information included in an error allows to
reexecute the single command sequence which caused the failure, and
also allows to generate a C program wich (hopefully) reproduces the
misbehavior.

Working with command sequences
------------------------------

Supposing that we have some command sequence produced by tests:
```
test :: [ProcCommand]
test = [([0],Accept 5),([1],ConnectTo "" 0 5 Nothing),([1,0],WaitConnection 5)
       ,([1],Send 5 0 (Msg 0 185)),([0],WaitRecv 5 0),([1],Send 5 1 (Msg 1 969))
       ,([1],WaitSendCompletion 5 0),([1],WaitSendCompletion 5 1)
       ,([1],Send 5 2 (Msg 2 495)),([1],WaitSendCompletion 5 2)
       ,([0],WaitRecv 5 1),([0],WaitRecv 5 2),([1],Disconnect 5)]
```
We can either execute it with
```
main :: IO ()
main = runCommands test >>= print
```
or we can generate a C program which executes it with:
```
main :: IO ()
main = writeFile "t.c" $ generateCTest test
```
