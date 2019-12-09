{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.RichEntry where

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

data RichEntry =
  RichEntry
    { reAmount  :: Int
    , rePusd    :: Int
    , reAddress :: Text
    } deriving (Show, Eq, GHC.Generics.Generic)

instance FromJSON RichEntry where
  parseJSON (Object v) =
    RichEntry
      <$> v .:  "amount"
      <*> v .:  "pusd"
      <*> v .:  "address"
  parseJSON _ = mzero

instance ToJSON RichEntry where
  toJSON (RichEntry {..}) =
    object [ "amount"  .= reAmount
           , "pusd"    .= rePusd
           , "address" .= reAddress
           ]
  toEncoding (RichEntry {..}) =
    pairs  (   "amount"  .= reAmount
            <> "pusd"    .= rePusd
            <> "address" .= reAddress)
