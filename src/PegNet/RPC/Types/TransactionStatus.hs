{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.TransactionStatus where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

--------------------------------------------------------------------------------

data TransactionStatus = TransactionStatus {
    topLevelHeight :: Int,
    topLevelExecuted :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TransactionStatus where
  parseJSON (Object v) = TransactionStatus <$> v .:  "height" <*> v .:  "executed"
  parseJSON _          = mzero


instance ToJSON TransactionStatus where
  toJSON     (TransactionStatus {..}) = object ["height" .= topLevelHeight, "executed" .= topLevelExecuted]
  toEncoding (TransactionStatus {..}) = pairs  ("height" .= topLevelHeight<>"executed" .= topLevelExecuted)
