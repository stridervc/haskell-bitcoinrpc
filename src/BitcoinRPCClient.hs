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

-- | Create a new client that must be passed to all bitcoin RPC functions
newBitcoinRPCClient :: RPCHost -> RPCPort -> RPCUsername -> RPCPassword -> BitcoinRPCClient
newBitcoinRPCClient host port username password
  = BitcoinRPCClient
  $ setRequestBasicAuth username password
  $ setRequestHost host
  $ setRequestPort port
  defaultRPCRequest

-- | Perform an RPC with given method name
callBitcoinRPC :: MonadIO m => BitcoinRPCClient -> RPCMethod -> m (Response (Either JSONException RPCResult))
callBitcoinRPC client method = httpRPC $ setRequestRPCMethod method $ request client

