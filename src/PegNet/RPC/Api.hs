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
endpointRemote = "http://dev.pegnet.org/v1"

-- | "get-sync-status"
--   Return the current heights synced by pegnetd and the factomd it is communicating with
reqGetSyncStatus :: RPC SyncStatus
reqGetSyncStatus =
  method "get-sync-status" None -- $ List [toJSON height]

-- | "get-transaction"
--   Returns if a given entry hash is a pegnet transaction, and if so returns the transaction.
reqGetTransaction :: Text -> Text -> RPC Transaction
reqGetTransaction chainId entryHash =
  method "get-transaction" $ List [String chainId, String entryHash]

-- | Get the total supply for each pegnet asset.
--
reqPegNetIssuance :: RPC NetIssuance
reqPegNetIssuance =
  method "get-pegnet-issuance" None

-- | Get the pegnet asset balances for a given address
--
reqPegNetBalances :: Text -> RPC NetBalances
reqPegNetBalances address =
  method "get-pegnet-balances" $ List [String address]

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
reqGeTransactions :: RPC [Transaction]
reqGeTransactions =
  method "get-transactions" None

--------------------------------------------------------------------------------

-- Testing access functionality
main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  h <- send s $ do
         h <- reqGetSyncStatus
         return h
  print h
