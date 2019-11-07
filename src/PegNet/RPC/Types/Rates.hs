{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module PegNet.RPC.Types.Rates where

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


data Rates = Rates {
    ratePBRL  :: Double,
    ratePRVN  :: Double,
    ratePEUR  :: Double,
    ratePUSD  :: Double,
    ratePEG   :: Double,
    ratePCHF  :: Double,
    ratePDCR  :: Double,
    ratePLTC  :: Double,
    ratePMXN  :: Double,
    ratePKRW  :: Double,
    ratePXBT  :: Double,
    ratePBNB  :: Double,
    ratePCAD  :: Double,
    ratePZEC  :: Double,
    ratePXAU  :: Double,
    ratePADA  :: Double,
    ratePCNY  :: Double,
    ratePXMR  :: Double,
    ratePHKD  :: Double,
    ratePINR  :: Double,
    ratePGBP  :: Double,
    ratePSGD  :: Double,
    ratePPHP  :: Double,
    ratePJPY  :: Double,
    ratePDASH :: Double,
    ratePXAG  :: Double,
    ratePXBC  :: Double,
    ratePFCT  :: Double,
    ratePXLM  :: Double,
    ratePETH  :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Rates where
  parseJSON (Object v) =
    Rates
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


instance ToJSON Rates where
  toJSON (Rates {..}) = object
    [ "pBRL" .= ratePBRL
    , "pRVN" .= ratePRVN
    , "pEUR" .= ratePEUR
    , "pUSD" .= ratePUSD
    , "PEG" .= ratePEG
    , "pCHF" .= ratePCHF
    , "pDCR" .= ratePDCR
    , "pLTC" .= ratePLTC
    , "pMXN" .= ratePMXN
    , "pKRW" .= ratePKRW
    , "pXBT" .= ratePXBT
    , "pBNB" .= ratePBNB
    , "pCAD" .= ratePCAD
    , "pZEC" .= ratePZEC
    , "pXAU" .= ratePXAU
    , "pADA" .= ratePADA
    , "pCNY" .= ratePCNY
    , "pXMR" .= ratePXMR
    , "pHKD" .= ratePHKD
    , "pINR" .= ratePINR
    , "pGBP" .= ratePGBP
    , "pSGD" .= ratePSGD
    , "pPHP" .= ratePPHP
    , "pJPY" .= ratePJPY
    , "pDASH" .= ratePDASH
    , "pXAG" .= ratePXAG
    , "pXBC" .= ratePXBC
    , "pFCT" .= ratePFCT
    , "pXLM" .= ratePXLM
    , "pETH" .= ratePETH
    ]
  toEncoding (Rates {..}) = pairs
    (  "pBRL"
    .= ratePBRL
    <> "pRVN"
    .= ratePRVN
    <> "pEUR"
    .= ratePEUR
    <> "pUSD"
    .= ratePUSD
    <> "PEG"
    .= ratePEG
    <> "pCHF"
    .= ratePCHF
    <> "pDCR"
    .= ratePDCR
    <> "pLTC"
    .= ratePLTC
    <> "pMXN"
    .= ratePMXN
    <> "pKRW"
    .= ratePKRW
    <> "pXBT"
    .= ratePXBT
    <> "pBNB"
    .= ratePBNB
    <> "pCAD"
    .= ratePCAD
    <> "pZEC"
    .= ratePZEC
    <> "pXAU"
    .= ratePXAU
    <> "pADA"
    .= ratePADA
    <> "pCNY"
    .= ratePCNY
    <> "pXMR"
    .= ratePXMR
    <> "pHKD"
    .= ratePHKD
    <> "pINR"
    .= ratePINR
    <> "pGBP"
    .= ratePGBP
    <> "pSGD"
    .= ratePSGD
    <> "pPHP"
    .= ratePPHP
    <> "pJPY"
    .= ratePJPY
    <> "pDASH"
    .= ratePDASH
    <> "pXAG"
    .= ratePXAG
    <> "pXBC"
    .= ratePXBC
    <> "pFCT"
    .= ratePFCT
    <> "pXLM"
    .= ratePXLM
    <> "pETH"
    .= ratePETH
    )
