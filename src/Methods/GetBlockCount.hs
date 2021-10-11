{-# LANGUAGE OverloadedStrings #-}

module Methods.GetBlockCount
  ( getBlockCount
  ) where

import RPC
import BitcoinRPCClient

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

-- | Returns the height of the most-work fully-validated chain
getBlockCount :: MonadIO m => BitcoinRPCClient -> m (Either Text Int)
getBlockCount client = callBitcoinRPC client "getblockcount" []
