This repository contains Haskell bindings for the CCI library.

See LICENSE for license information.

Building cci
============

Install latest cci first. You can get it from http://cci-forum.com/.

Then build and install the bindings:

    $ cabal install

You might need extra flags if CCI is not installed in a standard
location on your system:

    $ cabal install --extra-lib-dirs=/path/to/cci/lib \
                    --extra-include-dirs=/path/to/cci/include/

The `LD_LIBRARY_PATH` will also need to be set before running any
program or test in this case:

    $ export LD_LIBRARY_PATH=/path/to/cci/lib/

To generate HTML documentation:

    $ cabal haddock

Examples
========

pingpong
--------

Start the server:

    $ ./dist/build/ex-pingpong/ex-pingpong -s
    Opened ip://192.168.0.1:46188
    ...

Start the client using RMA transfers up to 4 MB:

    $ ./dist/build/ex-pingpong/ex-pingpong -h ip://192.168.0.1:46188 -r 4194304

Start the client using active messages:

    $ ./dist/build/ex-pingpong/ex-pingpong -h ip://192.168.0.1:46188

Tests
=====

In short:

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

See [test/README.md](test/README.md) for details.

Design notes
============

See [here](NOTES.md).
