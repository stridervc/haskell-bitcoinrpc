{-# LANGUAGE DuplicateRecordFields #-}
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
  , BitcoinRPCClient
  , getBlockCount
  , getBlockchainInfo
  , BlockchainInfo (..)
  , getBlock
  , Block (..)
  , BlockHash
  , TxID
  , getRawTransaction
  , Transaction (..)
  , VIn (..)
  , VOut (..)
  , ScriptSig (..)
  , ScriptPubKey (..)
  , getUptime
  ) where

import BitcoinRPCClient

import Methods.GetBlock
import Methods.GetBlockCount
import Methods.GetBlockchainInfo
import Methods.GetRawTransaction
import Methods.GetUptime
