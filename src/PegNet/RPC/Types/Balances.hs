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
import           Data.Int
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
    pBRL  :: Int64,
    pRVN  :: Int64,
    pEUR  :: Int64,
    pUSD  :: Int64,
    pPEG  :: Int64,
    pCHF  :: Int64,
    pDCR  :: Int64,
    pLTC  :: Int64,
    pMXN  :: Int64,
    pKRW  :: Int64,
    pXBT  :: Int64,
    pBNB  :: Int64,
    pCAD  :: Int64,
    pZEC  :: Int64,
    pXAU  :: Int64,
    pADA  :: Int64,
    pCNY  :: Int64,
    pXMR  :: Int64,
    pHKD  :: Int64,
    pINR  :: Int64,
    pGBP  :: Int64,
    pSGD  :: Int64,
    pPHP  :: Int64,
    pJPY  :: Int64,
    pDASH :: Int64,
    pXAG  :: Int64,
    pXBC  :: Int64,
    pFCT  :: Int64,
    pXLM  :: Int64,
    pETH  :: Int64
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
    [ "pBRL" .= pBRL
    , "pRVN" .= pRVN
    , "pEUR" .= pEUR
    , "pUSD" .= pUSD
    , "PEG"  .= pPEG
    , "pCHF" .= pCHF
    , "pDCR" .= pDCR
    , "pLTC" .= pLTC
    , "pMXN" .= pMXN
    , "pKRW" .= pKRW
    , "pXBT" .= pXBT
    , "pBNB" .= pBNB
    , "pCAD" .= pCAD
    , "pZEC" .= pZEC
    , "pXAU" .= pXAU
    , "pADA" .= pADA
    , "pCNY" .= pCNY
    , "pXMR" .= pXMR
    , "pHKD" .= pHKD
    , "pINR" .= pINR
    , "pGBP" .= pGBP
    , "pSGD" .= pSGD
    , "pPHP" .= pPHP
    , "pJPY" .= pJPY
    , "pDASH" .= pDASH
    , "pXAG" .= pXAG
    , "pXBC" .= pXBC
    , "pFCT" .= pFCT
    , "pXLM" .= pXLM
    , "pETH" .= pETH
    ]
  toEncoding (NetBalances {..}) = pairs
    (  "pBRL"
    .= pBRL
    <> "pRVN"
    .= pRVN
    <> "pEUR"
    .= pEUR
    <> "pUSD"
    .= pUSD
    <> "PEG"
    .= pPEG
    <> "pCHF"
    .= pCHF
    <> "pDCR"
    .= pDCR
    <> "pLTC"
    .= pLTC
    <> "pMXN"
    .= pMXN
    <> "pKRW"
    .= pKRW
    <> "pXBT"
    .= pXBT
    <> "pBNB"
    .= pBNB
    <> "pCAD"
    .= pCAD
    <> "pZEC"
    .= pZEC
    <> "pXAU"
    .= pXAU
    <> "pADA"
    .= pADA
    <> "pCNY"
    .= pCNY
    <> "pXMR"
    .= pXMR
    <> "pHKD"
    .= pHKD
    <> "pINR"
    .= pINR
    <> "pGBP"
    .= pGBP
    <> "pSGD"
    .= pSGD
    <> "pPHP"
    .= pPHP
    <> "pJPY"
    .= pJPY
    <> "pDASH"
    .= pDASH
    <> "pXAG"
    .= pXAG
    <> "pXBC"
    .= pXBC
    <> "pFCT"
    .= pFCT
    <> "pXLM"
    .= pXLM
    <> "pETH"
    .= pETH
    )
