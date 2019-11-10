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
import           Control.Exception                (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Network.Socket                   (HostName, ServiceName,
                                                   SocketType (Stream),
                                                   addrAddress, addrFamily,
                                                   addrProtocol, addrSocketType,
                                                   close, connect, defaultHints,
                                                   getAddrInfo, socket)

import           PegNet.RPC.Types.Balances
import           PegNet.RPC.Types.Issuance
import           PegNet.RPC.Types.Rates
import           PegNet.RPC.Types.SyncStatus
import           PegNet.RPC.Types.Transaction

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
reqGetTransaction chainId =
  method "get-transaction"
    $ Named [("txid", String chainId)]

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

-- |
--
reqPegNetRates :: Int -> RPC Rates
reqPegNetRates height =
  method "get-pegnet-rates" $ List [toJSON height]

-- |
--
reqGetTransactionStatus :: Text -> RPC ()
reqGetTransactionStatus id =
  method "get-transaction-status" None

-- |
--
reqGetTransactions :: RPC [Transaction]
reqGetTransactions =
  method "get-transactions" None

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
         b <- reqPegNetBalances "FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss"
         t <- reqGetTransaction "0-e4380e6334b0c42d4d6155fbd1378050b91c02a0df93d7fdfe6656f94c61e7eb"
         return (h, i)
  -- process resulted values
  --print h
  --print i
  return ()
