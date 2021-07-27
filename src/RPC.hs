{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Make RPC calls to Bitcoin server

module RPC
  ( RPCMethod
  , RPCResult (..)
  , defaultRPCRequest
  , setRequestRPCMethod
  , httpRPC
  ) where

import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson

type RPCMethod = T.Text

data RPCRequestBody = RPCRequestBody
  { jsonrpc :: T.Text
  , id      :: T.Text
  , method  :: RPCMethod
  } deriving (Eq, Show, Generic)

instance ToJSON RPCRequestBody

data RPCResult = RPCResult
  { result  :: Value
  , error   :: Maybe [(T.Text, Value)]
  -- , id      :: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON RPCResult

newRPCRequestBody :: RPCMethod -> RPCRequestBody
newRPCRequestBody m = RPCRequestBody  { jsonrpc = "1.0"
                                      , RPC.id  = "haskell-bitcoinrpc"
                                      , method  = m
                                      }

defaultRPCRequest :: Request
defaultRPCRequest
  = setRequestPath "/"
  $ setRequestMethod "POST"
  defaultRequest

setRequestRPCMethod :: RPCMethod -> Request -> Request
setRequestRPCMethod method = setRequestBodyJSON $ newRPCRequestBody method

httpRPC :: MonadIO m => Request -> m (Response RPCResult)
httpRPC = httpJSON

