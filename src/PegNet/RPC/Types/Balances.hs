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

      -- <*> v
      -- .:  "pAUD"
      -- <*> v
      -- .:  "pAED"

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
    pETH  :: Int64,
    --
    pNEO  :: Int64,
    pHT   :: Int64,
    pATOM :: Int64,
    pTZS  :: Int64,
    pRUB  :: Int64,
    pNZD  :: Int64,
    pLINK :: Int64,
    pARS  :: Int64,
    pRWF  :: Int64,
    pCRO  :: Int64,
    pZAR  :: Int64,
    pEOS  :: Int64,
    pDOGE :: Int64,
    pHBAR :: Int64,
    pBAT  :: Int64,
    pNGN  :: Int64,
    pTWD  :: Int64,
    pTRY  :: Int64,
    pVET  :: Int64,
    pETC  :: Int64,
    pNOK  :: Int64,
    pUGX  :: Int64,
    pSEK  :: Int64,
    pONT  :: Int64,
    pBIF  :: Int64,
    pALGO :: Int64,
    pKES  :: Int64,
    pETB  :: Int64,
    pXTZ  :: Int64,
    pGDB  :: Int64
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
      <*> v
      .:  "pNEO"
      <*> v
      .:  "pHT"
      <*> v
      .:  "pATOM"
      <*> v
      .:  "pTZS"
      <*> v
      .:  "pRUB"
      <*> v
      .:  "pNZD"
      <*> v
      .:  "pLINK"
      <*> v
      .:  "pARS"
      <*> v
      .:  "pRWF"
      <*> v
      .:  "pCRO"
      <*> v
      .:  "pZAR"
      <*> v
      .:  "pEOS"
      <*> v
      .:  "pDOGE"
      <*> v
      .:  "pHBAR"
      <*> v
      .:  "pBAT"
      <*> v
      .:  "pNGN"
      <*> v
      .:  "pTWD"
      <*> v
      .:  "pTRY"
      <*> v
      .:  "pVET"
      <*> v
      .:  "pETC"
      <*> v
      .:  "pNOK"
      <*> v
      .:  "pUGX"
      <*> v
      .:  "pSEK"
      <*> v
      .:  "pONT"
      <*> v
      .:  "pBIF"
      <*> v
      .:  "pALGO"
      <*> v
      .:  "pKES"
      <*> v
      .:  "pETB"
      <*> v
      .:  "pXTZ"
      <*> v
      .:  "pDGB"
  parseJSON _ = mzero


instance ToJSON NetBalances where
  toJSON (NetBalances {..}) = object
    [ "pBRL"  .= pBRL
    , "pRVN"  .= pRVN
    , "pEUR"  .= pEUR
    , "pUSD"  .= pUSD
    , "PEG"   .= pPEG
    , "pCHF"  .= pCHF
    , "pDCR"  .= pDCR
    , "pLTC"  .= pLTC
    , "pMXN"  .= pMXN
    , "pKRW"  .= pKRW
    , "pXBT"  .= pXBT
    , "pBNB"  .= pBNB
    , "pCAD"  .= pCAD
    , "pZEC"  .= pZEC
    , "pXAU"  .= pXAU
    , "pADA"  .= pADA
    , "pCNY"  .= pCNY
    , "pXMR"  .= pXMR
    , "pHKD"  .= pHKD
    , "pINR"  .= pINR
    , "pGBP"  .= pGBP
    , "pSGD"  .= pSGD
    , "pPHP"  .= pPHP
    , "pJPY"  .= pJPY
    , "pDASH" .= pDASH
    , "pXAG"  .= pXAG
    , "pXBC"  .= pXBC
    , "pFCT"  .= pFCT
    , "pXLM"  .= pXLM
    , "pETH"  .= pETH
    , "pNEO"  .= pNEO
    , "pHT"   .= pHT
    , "pATOM" .= pATOM
    , "pTZS"  .= pTZS
    , "pRUB"  .= pRUB
    , "pNZD"  .= pNZD
    , "pLINK" .= pLINK
    , "pARS"  .= pARS
    , "pRWF"  .= pRWF
    , "pCRO"  .= pCRO
    , "pZAR"  .= pZAR
    , "pEOS"  .= pEOS
    , "pDOGE" .= pDOGE
    , "pHBAR" .= pHBAR
    , "pBAT"  .= pBAT
    , "pNGN"  .= pNGN
    , "pTWD"  .= pTWD
    , "pTRY"  .= pTRY
    , "pVET"  .= pVET
    , "pETC"  .= pETC
    , "pNOK"  .= pNOK
    , "pUGX"  .= pUGX
    , "pSEK"  .= pSEK
    , "pONT"  .= pONT
    , "pBIF"  .= pBIF
    , "pALGO" .= pALGO
    , "pKES"  .= pKES
    , "pETB"  .= pETB
    , "pXTZ"  .= pXTZ
    , "pGDB"  .= pGDB
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
    <> "pNEO"
    .= pNEO
    <> "pHT"
    .= pHT
    <> "pATOM"
    .= pATOM
    <> "pTZS"
    .= pTZS
    <> "pRUB"
    .= pRUB
    <> "pNZD"
    .= pNZD
    <> "pLINK"
    .= pLINK
    <> "pARS"
    .= pARS
    <> "pRWF"
    .= pRWF
    <> "pCRO"
    .= pCRO
    <> "pZAR"
    .= pZAR
    <> "pEOS"
    .= pEOS
    <> "pDOGE"
    .= pDOGE
    <> "pHBAR"
    .= pHBAR
    <> "pBAT"
    .= pBAT
    <> "pNGN"
    .= pNGN
    <> "pTWD"
    .= pTWD
    <> "pTRY"
    .= pTRY
    <> "pVET"
    .= pVET
    <> "pETC"
    .= pETC
    <> "pNOK"
    .= pNOK
    <> "pUGX"
    .= pUGX
    <> "pSEK"
    .= pSEK
    <> "pONT"
    .= pONT
    <> "pBIF"
    .= pBIF
    <> "pALGO"
    .= pALGO
    <> "pKES"
    .= pKES
    <> "pETB"
    .= pETB
    <> "pXTZ"
    .= pXTZ
    <> "pGDB"
    .= pGDB
    )
