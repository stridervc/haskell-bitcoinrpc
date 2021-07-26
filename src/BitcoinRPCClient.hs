{-# LANGUAGE OverloadedStrings #-}

module BitcoinRPCClient
  ( BitcoinRPCClient (..)
  , newBitcoinRPCClient
  , callBitcoinRPC
  ) where

import RPC

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Network.HTTP.Simple

type RPCHost      = ByteString
type RPCPort      = Int 
type RPCUsername  = ByteString
type RPCPassword  = ByteString

newtype BitcoinRPCClient = BitcoinRPCClient { request :: Request }

newBitcoinRPCClient :: RPCHost -> RPCPort -> RPCUsername -> RPCPassword -> BitcoinRPCClient
newBitcoinRPCClient host port username password
  = BitcoinRPCClient
  $ setRequestBasicAuth username password
  $ setRequestHost host
  $ setRequestPort port
  defaultRPCRequest

callBitcoinRPC :: MonadIO m => BitcoinRPCClient -> RPCMethod -> m (Response RPCResult)
callBitcoinRPC client method = httpRPC $ setRequestRPCMethod method $ request client

