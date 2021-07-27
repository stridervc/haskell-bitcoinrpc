{-# LANGUAGE OverloadedStrings #-}

module Methods.GetBlockCount
  ( getBlockCount
  ) where

import RPC
import BitcoinRPCClient

import Control.Monad.IO.Class (MonadIO)
import Data.Scientific (toRealFloat)
import GHC.Float (float2Int)
import Network.HTTP.Simple
import Data.Aeson

getBlockCount :: MonadIO m => BitcoinRPCClient -> m Int
getBlockCount client = do
  response <- callBitcoinRPC client "getblockcount"
  let Number num = result $ getResponseBody response

  return $ float2Int $ toRealFloat num
