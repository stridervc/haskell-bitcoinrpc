{-# LANGUAGE OverloadedStrings #-}

module Methods.GetBlockHash
  ( getBlockHash
  ) where

import Types (BlockHash)
import BitcoinRPCClient

import Data.Aeson (toJSON)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

getBlockHash :: MonadIO m => BitcoinRPCClient -> Int -> m (Either Text BlockHash)
getBlockHash client height = callBitcoinRPC client "getblockhash" [toJSON height]
