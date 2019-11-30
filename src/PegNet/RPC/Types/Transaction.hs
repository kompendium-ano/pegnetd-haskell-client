{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.Transaction where

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


data Input = Input {
    inputAmount  :: Double,
    inputAddress :: Text,
    inputType    :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Input where
  parseJSON (Object v) = Input <$> v .:   "amount" <*> v .:   "address" <*> v .:   "type"
  parseJSON _          = mzero


instance ToJSON Input where
  toJSON     (Input {..}) = object ["amount" .= inputAmount, "address" .= inputAddress, "type" .= inputType]
  toEncoding (Input {..}) = pairs  ("amount" .= inputAmount<>"address" .= inputAddress<>"type" .= inputType)


data TransactionsElt = TransactionsElt {
    transactionsEltInput      :: Input,
    transactionsEltMetadata   :: (Maybe Value),
    transactionsEltConversion :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TransactionsElt where
  parseJSON (Object v) = TransactionsElt <$> v .:   "input" <*> v .:?? "metadata" <*> v .:   "conversion"
  parseJSON _          = mzero


instance ToJSON TransactionsElt where
  toJSON     (TransactionsElt {..}) = object ["input" .= transactionsEltInput, "metadata" .= transactionsEltMetadata, "conversion" .= transactionsEltConversion]
  toEncoding (TransactionsElt {..}) = pairs  ("input" .= transactionsEltInput<>"metadata" .= transactionsEltMetadata<>"conversion" .= transactionsEltConversion)


data Data = Data {
    dataTransactions :: [TransactionsElt],
    dataVersion      :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Data where
  parseJSON (Object v) = Data <$> v .:   "transactions" <*> v .:   "version"
  parseJSON _          = mzero


instance ToJSON Data where
  toJSON     (Data {..}) = object ["transactions" .= dataTransactions, "version" .= dataVersion]
  toEncoding (Data {..}) = pairs  ("transactions" .= dataTransactions<>"version" .= dataVersion)


data Transaction = Transaction {
    topLevelData      :: Data,
    topLevelEntryhash :: Text,
    topLevelTimestamp :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Transaction where
  parseJSON (Object v) = Transaction <$> v .:   "data" <*> v .:   "entryhash" <*> v .:   "timestamp"
  parseJSON _          = mzero


instance ToJSON Transaction where
  toJSON (Transaction {..}) =
    object [ "data"      .= topLevelData
           , "entryhash" .= topLevelEntryhash
           , "timestamp" .= topLevelTimestamp]
  toEncoding (Transaction {..}) =
    pairs  (   "data"      .= topLevelData
            <> "entryhash" .= topLevelEntryhash
            <>"timestamp"  .= topLevelTimestamp)
