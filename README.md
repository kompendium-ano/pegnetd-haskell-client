# Haskell JSON-RPC client for PegNet

[![Build Status](https://travis-ci.com/kompendium-llc/api-rpc-pegnet.svg?branch=master)](https://travis-ci.com/kompendium-llc/api-rpc-factom)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)

A JSON-RPC Haskell client for the PegNet [API](https://github.com/pegnet/pegnetd/wiki/API). Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

Choosing a batching strategy. There are two strategies:
- `Weak`   - a single command or a single procedure, or
- `Strong` - a sequence of commands, optionally terminated by a procedure

# Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-factom) and build with Cabal, but we recommend to use [Stack](https://haskellstack.org) tool. Add to you dependencies in stack.yaml and cabal file dependency `- api-rpc-factom`.

To run and test fromrepository

1. Build with stack
```bash
$ stack build
```

2. Load REPL with stack for evaluation
```
$ stack repl
```

3. execute required methods

## Usage

for basic daemon functionality

1. import with

```haskell
import PegNet.RPC.Api
```
or load in REPL.

2. build communication session with
```
weakSession (traceSendAPI "" $ clientSendAPI endpoint)
```

3. run required methods inside `RPC` monad

#### Retreiving a sync status

```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- run Remote Monad
h <- send s $ do
         -- run specific events by executing exposed
         h <- reqGetSyncStatus
         return h
-- show converted ADT
print h
-- or use for special business logic
```
