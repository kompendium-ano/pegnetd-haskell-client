{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.Issuance where

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


data Issuance = Issuance {
    issuancePBRL  :: Double,
    issuancePRVN  :: Double,
    issuancePEUR  :: Double,
    issuancePUSD  :: Double,
    issuancePEG   :: Double,
    issuancePCHF  :: Double,
    issuancePDCR  :: Double,
    issuancePLTC  :: Double,
    issuancePMXN  :: Double,
    issuancePKRW  :: Double,
    issuancePXBT  :: Double,
    issuancePBNB  :: Double,
    issuancePCAD  :: Double,
    issuancePZEC  :: Double,
    issuancePXAU  :: Double,
    issuancePADA  :: Double,
    issuancePCNY  :: Double,
    issuancePXMR  :: Double,
    issuancePHKD  :: Double,
    issuancePINR  :: Double,
    issuancePGBP  :: Double,
    issuancePSGD  :: Double,
    issuancePPHP  :: Double,
    issuancePJPY  :: Double,
    issuancePDASH :: Double,
    issuancePXAG  :: Double,
    issuancePXBC  :: Double,
    issuancePFCT  :: Double,
    issuancePXLM  :: Double,
    issuancePETH  :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Issuance where
  parseJSON (Object v) =
    Issuance
      <$> v
      .:  "pBRL"
      <*> v
      .:  "pRVN"
      <*> v
      .:  "pEUR"
      <*> v
      .:  "pUSD"
      <*> v
      .:  "PEG"
      <*> v
      .:  "pCHF"
      <*> v
      .:  "pDCR"
      <*> v
      .:  "pLTC"
      <*> v
      .:  "pMXN"
      <*> v
      .:  "pKRW"
      <*> v
      .:  "pXBT"
      <*> v
      .:  "pBNB"
      <*> v
      .:  "pCAD"
      <*> v
      .:  "pZEC"
      <*> v
      .:  "pXAU"
      <*> v
      .:  "pADA"
      <*> v
      .:  "pCNY"
      <*> v
      .:  "pXMR"
      <*> v
      .:  "pHKD"
      <*> v
      .:  "pINR"
      <*> v
      .:  "pGBP"
      <*> v
      .:  "pSGD"
      <*> v
      .:  "pPHP"
      <*> v
      .:  "pJPY"
      <*> v
      .:  "pDASH"
      <*> v
      .:  "pXAG"
      <*> v
      .:  "pXBC"
      <*> v
      .:  "pFCT"
      <*> v
      .:  "pXLM"
      <*> v
      .:  "pETH"
  parseJSON _ = mzero


instance ToJSON Issuance where
  toJSON (Issuance {..}) = object
    [ "pBRL" .= issuancePBRL
    , "pRVN" .= issuancePRVN
    , "pEUR" .= issuancePEUR
    , "pUSD" .= issuancePUSD
    , "PEG" .= issuancePEG
    , "pCHF" .= issuancePCHF
    , "pDCR" .= issuancePDCR
    , "pLTC" .= issuancePLTC
    , "pMXN" .= issuancePMXN
    , "pKRW" .= issuancePKRW
    , "pXBT" .= issuancePXBT
    , "pBNB" .= issuancePBNB
    , "pCAD" .= issuancePCAD
    , "pZEC" .= issuancePZEC
    , "pXAU" .= issuancePXAU
    , "pADA" .= issuancePADA
    , "pCNY" .= issuancePCNY
    , "pXMR" .= issuancePXMR
    , "pHKD" .= issuancePHKD
    , "pINR" .= issuancePINR
    , "pGBP" .= issuancePGBP
    , "pSGD" .= issuancePSGD
    , "pPHP" .= issuancePPHP
    , "pJPY" .= issuancePJPY
    , "pDASH" .= issuancePDASH
    , "pXAG" .= issuancePXAG
    , "pXBC" .= issuancePXBC
    , "pFCT" .= issuancePFCT
    , "pXLM" .= issuancePXLM
    , "pETH" .= issuancePETH
    ]
  toEncoding (Issuance {..}) = pairs
    (  "pBRL"
    .= issuancePBRL
    <> "pRVN"
    .= issuancePRVN
    <> "pEUR"
    .= issuancePEUR
    <> "pUSD"
    .= issuancePUSD
    <> "PEG"
    .= issuancePEG
    <> "pCHF"
    .= issuancePCHF
    <> "pDCR"
    .= issuancePDCR
    <> "pLTC"
    .= issuancePLTC
    <> "pMXN"
    .= issuancePMXN
    <> "pKRW"
    .= issuancePKRW
    <> "pXBT"
    .= issuancePXBT
    <> "pBNB"
    .= issuancePBNB
    <> "pCAD"
    .= issuancePCAD
    <> "pZEC"
    .= issuancePZEC
    <> "pXAU"
    .= issuancePXAU
    <> "pADA"
    .= issuancePADA
    <> "pCNY"
    .= issuancePCNY
    <> "pXMR"
    .= issuancePXMR
    <> "pHKD"
    .= issuancePHKD
    <> "pINR"
    .= issuancePINR
    <> "pGBP"
    .= issuancePGBP
    <> "pSGD"
    .= issuancePSGD
    <> "pPHP"
    .= issuancePPHP
    <> "pJPY"
    .= issuancePJPY
    <> "pDASH"
    .= issuancePDASH
    <> "pXAG"
    .= issuancePXAG
    <> "pXBC"
    .= issuancePXBC
    <> "pFCT"
    .= issuancePFCT
    <> "pXLM"
    .= issuancePXLM
    <> "pETH"
    .= issuancePETH
    )


data Syncstatus = Syncstatus {
    syncstatusFactomheight :: Double,
    syncstatusSyncheight   :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Syncstatus where
  parseJSON (Object v) =
    Syncstatus <$> v .: "factomheight" <*> v .: "syncheight"
  parseJSON _ = mzero


instance ToJSON Syncstatus where
  toJSON (Syncstatus {..}) = object
    [ "factomheight" .= syncstatusFactomheight
    , "syncheight" .= syncstatusSyncheight
    ]
  toEncoding (Syncstatus {..}) = pairs
    (  "factomheight"
    .= syncstatusFactomheight
    <> "syncheight"
    .= syncstatusSyncheight
    )


data NetIssuance = NetIssuance {
    topLevelIssuance   :: Issuance,
    topLevelSyncstatus :: Syncstatus
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON NetIssuance where
  parseJSON (Object v) = NetIssuance <$> v .: "issuance" <*> v .: "syncstatus"
  parseJSON _          = mzero


instance ToJSON NetIssuance where
  toJSON (NetIssuance {..}) =
    object ["issuance" .= topLevelIssuance, "syncstatus" .= topLevelSyncstatus]
  toEncoding (NetIssuance {..}) =
    pairs ("issuance" .= topLevelIssuance <> "syncstatus" .= topLevelSyncstatus)
