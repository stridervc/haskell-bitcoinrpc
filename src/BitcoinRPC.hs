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
import Network.HTTP.Simple

getBlockCount :: MonadIO m => ByteString -> Int -> ByteString -> ByteString -> m (Response RPCResult)
getBlockCount host port username password = do
  let request = setRequestRPCMethod "getblockcount"
              $ setRequestBasicAuth username password
              $ setRequestHost host
              $ setRequestPort port
              defaultRPCRequest

  httpRPC request

