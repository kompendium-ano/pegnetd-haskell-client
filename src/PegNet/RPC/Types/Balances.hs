{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.Balances where

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


data NetBalances = NetBalances {
    topLevelPBRL  :: Double,
    topLevelPRVN  :: Double,
    topLevelPEUR  :: Double,
    topLevelPUSD  :: Double,
    topLevelPEG   :: Double,
    topLevelPCHF  :: Double,
    topLevelPDCR  :: Double,
    topLevelPLTC  :: Double,
    topLevelPMXN  :: Double,
    topLevelPKRW  :: Double,
    topLevelPXBT  :: Double,
    topLevelPBNB  :: Double,
    topLevelPCAD  :: Double,
    topLevelPZEC  :: Double,
    topLevelPXAU  :: Double,
    topLevelPADA  :: Double,
    topLevelPCNY  :: Double,
    topLevelPXMR  :: Double,
    topLevelPHKD  :: Double,
    topLevelPINR  :: Double,
    topLevelPGBP  :: Double,
    topLevelPSGD  :: Double,
    topLevelPPHP  :: Double,
    topLevelPJPY  :: Double,
    topLevelPDASH :: Double,
    topLevelPXAG  :: Double,
    topLevelPXBC  :: Double,
    topLevelPFCT  :: Double,
    topLevelPXLM  :: Double,
    topLevelPETH  :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON NetBalances where
  parseJSON (Object v) =
    NetBalances
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


instance ToJSON NetBalances where
  toJSON (NetBalances {..}) = object
    [ "pBRL" .= topLevelPBRL
    , "pRVN" .= topLevelPRVN
    , "pEUR" .= topLevelPEUR
    , "pUSD" .= topLevelPUSD
    , "PEG" .= topLevelPEG
    , "pCHF" .= topLevelPCHF
    , "pDCR" .= topLevelPDCR
    , "pLTC" .= topLevelPLTC
    , "pMXN" .= topLevelPMXN
    , "pKRW" .= topLevelPKRW
    , "pXBT" .= topLevelPXBT
    , "pBNB" .= topLevelPBNB
    , "pCAD" .= topLevelPCAD
    , "pZEC" .= topLevelPZEC
    , "pXAU" .= topLevelPXAU
    , "pADA" .= topLevelPADA
    , "pCNY" .= topLevelPCNY
    , "pXMR" .= topLevelPXMR
    , "pHKD" .= topLevelPHKD
    , "pINR" .= topLevelPINR
    , "pGBP" .= topLevelPGBP
    , "pSGD" .= topLevelPSGD
    , "pPHP" .= topLevelPPHP
    , "pJPY" .= topLevelPJPY
    , "pDASH" .= topLevelPDASH
    , "pXAG" .= topLevelPXAG
    , "pXBC" .= topLevelPXBC
    , "pFCT" .= topLevelPFCT
    , "pXLM" .= topLevelPXLM
    , "pETH" .= topLevelPETH
    ]
  toEncoding (NetBalances {..}) = pairs
    (  "pBRL"
    .= topLevelPBRL
    <> "pRVN"
    .= topLevelPRVN
    <> "pEUR"
    .= topLevelPEUR
    <> "pUSD"
    .= topLevelPUSD
    <> "PEG"
    .= topLevelPEG
    <> "pCHF"
    .= topLevelPCHF
    <> "pDCR"
    .= topLevelPDCR
    <> "pLTC"
    .= topLevelPLTC
    <> "pMXN"
    .= topLevelPMXN
    <> "pKRW"
    .= topLevelPKRW
    <> "pXBT"
    .= topLevelPXBT
    <> "pBNB"
    .= topLevelPBNB
    <> "pCAD"
    .= topLevelPCAD
    <> "pZEC"
    .= topLevelPZEC
    <> "pXAU"
    .= topLevelPXAU
    <> "pADA"
    .= topLevelPADA
    <> "pCNY"
    .= topLevelPCNY
    <> "pXMR"
    .= topLevelPXMR
    <> "pHKD"
    .= topLevelPHKD
    <> "pINR"
    .= topLevelPINR
    <> "pGBP"
    .= topLevelPGBP
    <> "pSGD"
    .= topLevelPSGD
    <> "pPHP"
    .= topLevelPPHP
    <> "pJPY"
    .= topLevelPJPY
    <> "pDASH"
    .= topLevelPDASH
    <> "pXAG"
    .= topLevelPXAG
    <> "pXBC"
    .= topLevelPXBC
    <> "pFCT"
    .= topLevelPFCT
    <> "pXLM"
    .= topLevelPXLM
    <> "pETH"
    .= topLevelPETH
    )
