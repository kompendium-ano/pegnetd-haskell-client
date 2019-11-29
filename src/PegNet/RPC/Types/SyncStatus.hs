{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.SyncStatus where

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

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data SyncStatus = SyncStatus {
    topLevelFactomheight :: Double,
    topLevelSyncheight   :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)

instance FromJSON SyncStatus where
  parseJSON (Object v) =
    SyncStatus
      <$> v .:   "factomheight"
      <*> v .:   "syncheight"
  parseJSON _          = mzero

instance ToJSON SyncStatus where
  toJSON (SyncStatus {..}) =
    object [ "factomheight" .= topLevelFactomheight
           , "syncheight"   .= topLevelSyncheight]
  toEncoding (SyncStatus {..}) =
    pairs  (   "factomheight" .= topLevelFactomheight
            <> "syncheight"   .= topLevelSyncheight)
