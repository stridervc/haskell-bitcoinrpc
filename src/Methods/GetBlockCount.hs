{-# LANGUAGE OverloadedStrings #-}

module Methods.GetBlockCount
  ( getBlockCount
  ) where

import RPC
import BitcoinRPCClient

import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO)
import Data.Scientific (toRealFloat)
import GHC.Float (float2Int)
import Network.HTTP.Simple
import Data.Aeson

-- | Returns the height of the most-work fully-validated chain
getBlockCount :: MonadIO m => BitcoinRPCClient -> m (Either Text Int)
getBlockCount client = callBitcoinRPC client "getblockcount" []
