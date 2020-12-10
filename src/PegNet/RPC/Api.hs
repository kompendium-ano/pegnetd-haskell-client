{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module PegNet.RPC.Api where

import           Control.Concurrent
import           Control.Exception                  (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Function                      (on)
import           Data.List                          (sort, sortBy)
import           Data.Ord                           (comparing)
import           Data.Text
import           Network.Socket                     (HostName, ServiceName,
                                                     SocketType (Stream),
                                                     addrAddress, addrFamily,
                                                     addrProtocol,
                                                     addrSocketType, close,
                                                     connect, defaultHints,
                                                     getAddrInfo, socket)

import           PegNet.RPC.Types.Balances
import           PegNet.RPC.Types.Issuance
import           PegNet.RPC.Types.Rates
import           PegNet.RPC.Types.RichEntry
import           PegNet.RPC.Types.SyncStatus
import           PegNet.RPC.Types.Transaction
import           PegNet.RPC.Types.TransactionStatus

--------------------------------------------------------------------------------

-- | Simple endpoint wrappers to use as different
--   local or remotte API endoint, keep default values
endpoint         = "http://localhost:8070/v1"
endpointRemote   = "http://51.158.171.20:8070/v1"
endpointOpenNode = "https://api.pegnetd.com"

-- | "get-sync-status"
--   Return the current heights synced by pegnetd and the factomd it is communicating with
reqGetSyncStatus :: RPC SyncStatus
reqGetSyncStatus =
  method "get-sync-status" None

-- | "get-transaction"
--   Returns the given pegnet transaction if it exists.
reqGetTransaction :: Text             -- ^ Transaction id
                  -> RPC Transaction  -- ^ Requested Transaction
reqGetTransaction txid =
  method "get-transaction"
    $ Named [("txid", String txid)]

-- | Get the total supply for each pegnet asset.
--
reqPegNetIssuance :: RPC NetIssuance
reqPegNetIssuance =
  method "get-pegnet-issuance" None

-- | Get the pegnet asset balances for a given address
--
reqPegNetBalances :: Text             -- ^ Address to get balances from
                  -> RPC NetBalances  -- ^ Resulting balances over all assets
reqPegNetBalances address =
  method "get-pegnet-balances"
    $ Named [("address", String address)]

-- | "get-pegnet-rates"
--   Returns the pegnet conversion rates for a given block height.
reqPegNetRates :: Int              -- ^ Specified height to get rates at
               -> RPC NetBalances  -- ^ Resulted Rates
reqPegNetRates height =
  method "get-pegnet-rates"
    $ Named [("height", toJSON height)]

-- | "get-transaction-status"
--   Returns the status of a transaction.
--   The parameter `entryhash` can be either a winning OPR entry hash,
--   a transaction chain entry hash, or an FCT Burn transaction id.
reqGetTransactionStatus :: Text                    -- ^ Transaction chain entry hash
                        -> RPC TransactionStatus   -- ^ Current status of transaction
reqGetTransactionStatus entryHash =
  method "get-transaction-status"
    $ Named [("entryhash", String entryHash)]

-- | get-transactions
--   Returns a set of up to 50 transactions for the given parameters.
--   TODO: this many arguments including boolean combination is a case
--         for a bad API design. Better hide it with convinience functions
--         provided to library user
reqGetTransactions :: Maybe Text        -- ^
                   -> Maybe Text        -- ^
                   -> Maybe Int         -- ^
                   -> Maybe Int         -- ^ offset
                   -> Maybe Bool        -- ^
                   -> Maybe Bool        -- ^
                   -> Maybe Bool        -- ^
                   -> Maybe Bool        -- ^
                   -> Maybe Bool        -- ^
                   -> RPC [Transaction] -- ^ List of Transactions by specified parameters
reqGetTransactions mbEntryHash mbAddress mbHeight mbOffset b1 b2 b3 b4 b5 =
  method "get-transactions"
    $ Named ([]
     ++ (case mbEntryHash of
           Nothing -> []
           Just eh -> [("entryhash", String eh)])
     ++ (case mbAddress of
           Nothing -> []
           Just ad -> [("address", String ad)])
     ++ (case mbHeight of
           Nothing -> []
           Just hg -> [("height", toJSON hg)]))


-- | "get-rich-list"
--   Returns the rich list of addresses for all assets or a specific asset.
reqGetRichList :: Maybe Text  -- ^ Asset name, exclude to list by all assets
               -> Int         -- ^ Number to limit from top
               -> RPC [RichEntry]
reqGetRichList mbAsset limit =
  method "get-rich-list"
    $ Named ([("count", toJSON limit)]
             ++ (case mbAsset of
                   Nothing    -> []
                   Just asset -> [("asset", String asset)]))
