{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetMempoolInfo
  ( getMempoolInfo
  , MempoolInfo (..)
  ) where

import BitcoinRPCClient
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Control.Monad.IO.Class (MonadIO)

data MempoolInfo = MempoolInfo
  { loaded            :: Bool
  , size              :: Int
  , bytes             :: Int
  , usage             :: Int
  , total_fee         :: Float
  , maxmempool        :: Int
  , mempoolminfee     :: Float
  , minrelaytxfee     :: Float
  , unbroadcastcount  :: Int
  , fullrbf           :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON MempoolInfo

getMempoolInfo :: MonadIO m => BitcoinRPCClient -> m (Either Text MempoolInfo)
getMempoolInfo client = callBitcoinRPC client "getmempoolinfo" []
