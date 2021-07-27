{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetBlockchainInfo
  ( BlockchainInfo (..)
  , SoftFork (..)
  , getBlockchainInfo
  ) where

import RPC
import BitcoinRPCClient

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Map (Map)
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson

data ForkType = Buried | Bip9 | UnknownForkType deriving (Eq, Show)

instance FromJSON ForkType where
  parseJSON (String text) =
    case text of
      "buried"  -> return Buried
      "bip9"    -> return Bip9
      _         -> return UnknownForkType
  parseJSON _ = undefined

data Bip9Status = Bip9Defined | Bip9Started | Bip9LockedIn | Bip9Active | Bip9Failed | Bip9Unknown deriving (Eq, Show)

instance FromJSON Bip9Status where
  parseJSON (String text) =
    case text of
      "defined"   -> return Bip9Defined
      "started"   -> return Bip9Started
      "locked_in" -> return Bip9LockedIn
      "active"    -> return Bip9Active
      "failed"    -> return Bip9Failed
      _           -> return Bip9Unknown
  parseJSON _ = undefined

data Bip9Statistics = Bip9Statistics
  { period    :: Int
  , threshold :: Int
  , elapsed   :: Int
  , count     :: Int
  , possible  :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON Bip9Statistics

data Bip9Info = Bip9Info
  { status    :: Bip9Status
  , bit       :: Maybe Int
  , start_time  :: Int
  , timeout     :: Int
  , since       :: Int
  , statistics  :: Maybe Bip9Statistics
  } deriving (Eq, Show, Generic)

instance FromJSON Bip9Info

data SoftFork = SoftFork
  { forktype  :: ForkType
  , active    :: Bool
  , height    :: Maybe Int
  , bip9      :: Maybe Bip9Info
  } deriving (Eq, Show, Generic)

instance FromJSON SoftFork where
  parseJSON (Object o) =
    SoftFork  <$> o .:  "type"
              <*> o .:  "active"
              <*> o .:? "height"
              <*> o .:? "bip9"
  parseJSON _ = undefined

data BlockchainInfo = BlockchainInfo
  { chain                 :: Text
  , blocks                :: Int
  , bestblockhash         :: Text
  , difficulty            :: Float
  , mediantime            :: Int
  , verificationprogress  :: Float
  , initialblockdownload  :: Bool
  , chainwork             :: Text
  , size_on_disk          :: Int
  , pruned                :: Bool
  , softforks             :: Maybe (Map Text SoftFork)
  , warnings              :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON BlockchainInfo

getBlockchainInfo :: MonadIO m => BitcoinRPCClient -> m (Either String BlockchainInfo)
getBlockchainInfo client = do
  response <- callBitcoinRPC client "getblockchaininfo"
  -- let Just res = decode $ encode $ result $ getResponseBody response
  let res = eitherDecode $ encode $ result $ getResponseBody response
  return res

