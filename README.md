# pads-haskell
The pads haskell repository contains the code for the Haskell binding for PADS.  For more information about the project, see the 
pads website (www.padsproj.org). 

# Building

`pads-haskell` currently requires GHC 7.10.3. This project provides an
appropriate [Stack][1] configuration file.

## Setup

To install an appropriate GHC tool chain:

```bash
$ stack setup
```

## Build

To build `pads-haskell`:

```bash
$ stack build
```

## Test

To test `pads-haskell`:

```bash
$ stack test
```

[1]: https://www.stackage.org/
