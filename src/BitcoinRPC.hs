{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BitcoinRPC
  ( getBlockCount
  ) where

import RPC

import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Scientific (toRealFloat)
import GHC.Float (float2Int)
import Network.HTTP.Simple
import Data.Aeson

getBlockCount :: MonadIO m => ByteString -> Int -> ByteString -> ByteString -> m Int
getBlockCount host port username password = do
  let request = setRequestRPCMethod "getblockcount"
              $ setRequestBasicAuth username password
              $ setRequestHost host
              $ setRequestPort port
              defaultRPCRequest

  response <- httpRPC request
  let body = getResponseBody response
  let Number num = result body
  return $ float2Int $ toRealFloat num

