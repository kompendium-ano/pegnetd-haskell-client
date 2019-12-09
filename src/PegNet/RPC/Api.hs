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
endpoint       = "http://localhost:8070/v1"
endpointRemote = "https://api.pegnetd.com"

-- | "get-sync-status"
--   Return the current heights synced by pegnetd and the factomd it is communicating with
reqGetSyncStatus :: RPC SyncStatus
reqGetSyncStatus =
  method "get-sync-status" None -- $ List [toJSON height]

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
reqPegNetRates :: Int           -- ^ Specified height to get rates at
               -> RPC Rates     -- ^ Resulted Rates
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


--------------------------------------------------------------------------------

-- Testing access functionality
-- NB: Factom and PegNet API return results with `plain\text` headers, we
--     need to use alternative client to handle conversion issues, since Wreq
--     library automatically throws and error for responses that are not `application\json`
main = do
  let s = weakSession $ traceSendAPI "" $ clientSendAPIWithAlt endpointRemote
  (h, i) <- send s $ do
         h <- reqGetSyncStatus
         i <- reqPegNetIssuance
         --b <- reqPegNetBalances "FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss"
         --t <- reqGetTransaction "0-e4380e6334b0c42d4d6155fbd1378050b91c02a0df93d7fdfe6656f94c61e7eb"
         --r <- reqPegNetRates 213000
         -- s <- reqGetTransactionStatus "a33d4f334a2658c17d3f44158af75f1c32cc6b2f3de9ddc337064c93043d8db0"
         rich <- reqGetRichList (Just "PEG") 5
         return (h, i)
  -- process resulted values
  --print h
  --print i
  return ()
