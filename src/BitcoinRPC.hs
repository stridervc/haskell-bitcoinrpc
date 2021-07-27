{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : haskell-bitcoinrpc
Description : Haskell module to communicate with a Bitcoin node
Copyright   : Fred Strauss 2021
License     : TBD
Maintainer  : fred@strider.co.za
Stability   : experimental

This module is a work in progress. Currently I'm implementing RPC methods as I need them.

-}

module BitcoinRPC
  ( newBitcoinRPCClient
  , getBlockCount
  , getBlockchainInfo
  ) where

import BitcoinRPCClient

import Methods.GetBlockCount
import Methods.GetBlockchainInfo
