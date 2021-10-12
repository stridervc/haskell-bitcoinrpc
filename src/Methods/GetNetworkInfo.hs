{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetNetworkInfo
  ( NetworkInfo (..)
  , Network (..)
  , LocalAddress (..)
  , getNetworkInfo
  ) where

import RPC
import Types
import BitcoinRPCClient

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

data Network = Network
  { name                        :: Text
  , limited                     :: Bool
  , reachable                   :: Bool
  , proxy                       :: Text
  , proxy_randomize_credentials :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON Network

data LocalAddress = LocalAddress
  { address   :: Text
  , port      :: Int
  , score     :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON LocalAddress

data NetworkInfo = NetworkInfo
  { version             :: Integer
  , subversion          :: Text
  , protocolversion     :: Integer
  , localservices       :: Text
  , localservicesnames  :: [Text]
  , localrelay          :: Bool
  , timeoffset          :: Int
  , connections         :: Int
  , connections_in      :: Int
  , connections_out     :: Int
  , networkactive       :: Bool
  , networks            :: [Network]
  , relayfee            :: Float
  , incrementalfee      :: Float
  , localaddresses      :: [LocalAddress]
  , warnings            :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON NetworkInfo

-- | Get network info
getNetworkInfo :: MonadIO m => BitcoinRPCClient -> m (Either Text NetworkInfo)
getNetworkInfo client = callBitcoinRPC client "getnetworkinfo" []

