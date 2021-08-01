{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Make RPC calls to Bitcoin server

module RPC
  ( RPCMethod
  , RPCResult (..)
  , defaultRPCRequest
  , setRequestRPCMethod
  , httpRPC
  , getErrorOrValue
  ) where

import Data.Text (Text, pack)

import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson

type RPCMethod = Text

data RPCRequestBody = RPCRequestBody
  { jsonrpc :: Text
  , id      :: Text
  , method  :: RPCMethod
  } deriving (Eq, Show, Generic)

instance ToJSON RPCRequestBody

data RPCResult = RPCResult
  { result  :: Value
  , error   :: Maybe [(Text, Value)]
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

httpRPC :: MonadIO m => Request -> m (Response (Either JSONException RPCResult))
httpRPC = httpJSONEither

getErrorOrValue :: Response (Either JSONException RPCResult) -> Either Text Value
getErrorOrValue response = do
  case getResponseStatus response of
    ok200 ->  do
                case getResponseBody response of
                  Left e    -> Left $ pack $ show e
                  Right res -> Right $ result res
