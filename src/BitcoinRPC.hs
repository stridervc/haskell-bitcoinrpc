{-# LANGUAGE OverloadedStrings #-}

module BitcoinRPC
  ( newBitcoinRPCClient
  , getBlockCount
  , getBlockchainInfo
  ) where

import BitcoinRPCClient

import Methods.GetBlockCount
import Methods.GetBlockchainInfo
