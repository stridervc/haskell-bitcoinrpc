{-# LANGUAGE OverloadedStrings #-}

module Methods.GetRawMempool
  ( getRawMempool
  ) where

import Types (TxID)
import BitcoinRPCClient
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

getRawMempool :: MonadIO m => BitcoinRPCClient -> m (Either Text [TxID])
getRawMempool client = callBitcoinRPC client "getrawmempool" []
