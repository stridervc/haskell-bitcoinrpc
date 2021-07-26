{-# LANGUAGE OverloadedStrings #-}

module BitcoinRPC
  ( newBitcoinRPCClient
  , getBlockCount
  ) where

import BitcoinRPCClient
import Methods.GetBlockCount
