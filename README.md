# Haskell JSON-RPC client for PegNet daemon

![Hackage](https://img.shields.io/hackage/v/api-rpc-pegnet)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)

A JSON-RPC Haskell client for the PegNet [API](https://github.com/pegnet/pegnetd/wiki/API) daemon. Allows retrieval of data from PegNet dApp. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

Choosing a batching strategy. There are two strategies:
- `Weak`   - a single command or a single procedure, or
- `Strong` - a sequence of commands, optionally terminated by a procedure

1. [Installation](#installation)
2. [Usage](#usage)
3. [Contributions](#contributions)

## Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-pegnet) and build with Cabal, but we recommend to use [Stack](https://haskellstack.org) tool. Add to you dependencies in stack.yaml and cabal file dependency `- api-rpc-pegnet`.

To run and test from repository:

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
weakSession (traceSendAPI "" $ clientSendAPIWithAlt endpoint)
```

3. run required methods inside `RPC` monad

## Usage

You can construct simple session with all methods available for further processings. Check following execution

```haskell
main = do
  -- build communication session
  let s = weakSession $ traceSendAPI "" $ clientSendAPIWithAlt endpointRemote
  (h, i) <- send s $ do
         h <- reqGetSyncStatus
         i <- reqPegNetIssuance
         b <- reqPegNetBalances "FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss"
         t <- reqGetTransaction "0-e4380e6334b0c42d4d6155fbd1378050b91c02a0df93d7fdfe6656f94c61e7eb"
         r <- reqPegNetRates 213000
         s <- reqGetTransactionStatus "a33d4f334a2658c17d3f44158af75f1c32cc6b2f3de9ddc337064c93043d8db0"
         rich <- reqGetRichList (Just "PEG") 5

         -- return values as N-ary tuple
         return (h, i)
  -- process resulted values
  print h
  -- or use for special business logic
```

Variables `h` and `i` mapped to exact data stucture. All types available for detailed overview in `Types` directory.

## Contributions

Please, feel free to contribute, fork the repo and submit PR.


Say thanks, send a tip:

- `btc`: 39oVXpsgsyW8ZgzsnX3sV7HLdtXWfT96qN
- `fct`: FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss
- `eth`: 0x9cDBA6bb44772259B3A3fb89cf233A147a720f34
- `xlm`: GD2FROTYXYSNOQQREYXHCXVOYWV556EM6PAHMVUHJNJEUYTUXUSNUZQ3
