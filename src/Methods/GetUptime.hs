{-# LANGUAGE OverloadedStrings #-}

module Methods.GetUptime
  ( getUptime
  ) where

import RPC
import BitcoinRPCClient

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

-- | Returns the uptime of the daemon in seconds
getUptime :: MonadIO m => BitcoinRPCClient -> m (Either Text Int)
getUptime client = callBitcoinRPC client "uptime" []
