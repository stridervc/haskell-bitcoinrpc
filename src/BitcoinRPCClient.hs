{-# LANGUAGE OverloadedStrings #-}

module BitcoinRPCClient
  ( BitcoinRPCClient (..)
  , newBitcoinRPCClient
  , callBitcoinRPC
  ) where

import RPC

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (try, displayException)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Network.HTTP.Simple
import Data.Aeson

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

-- | Perform an RPC with given method name and parameters
callBitcoinRPC :: (MonadIO m, FromJSON a) => BitcoinRPCClient -> RPCMethod -> [RPCParam] -> m (Either Text a)
callBitcoinRPC client method params = do
  tryresponse <- liftIO $ try $ httpRPC $ setRequestRPCMethod method params $ request client
  case tryresponse of
    Left e          -> return $ Left $ pack $ displayException (e :: HttpException)
    Right response  -> case getResponseBody response of
                        Left e      -> return $ Left $ pack $ show e
                        Right body  -> case fromJSON $ result body of
                                        Error e     -> return $ Left $ pack e
                                        Success res -> return $ Right res
