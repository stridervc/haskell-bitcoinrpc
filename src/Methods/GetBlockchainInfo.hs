{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetBlockchainInfo
  ( BlockchainInfo (..)
  , SoftFork (..)
  , Bip9Statistics (..)
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

data Bip9Statistics = Bip9Statistics
  { period    :: Int  -- ^ The length in blocks of the BIP9 signalling period
  , threshold :: Int  -- ^ The number of blocks required to activate the feature
  , elapsed   :: Int  -- ^ The number of blocks elapsed since the beginning of the current period
  , count     :: Int  -- ^ The number of blocks signalling in the current period
  , possible  :: Bool -- ^ False if not enough blocks left in the current period to pass threshold
  } deriving (Eq, Show, Generic)

instance FromJSON Bip9Statistics

-- | Only applicable for BIP9 type softforks
data Bip9Info = Bip9Info
  { status      :: Text       -- ^ "defined", "started", "locked_in", "active" or "failed"
  , bit         :: Maybe Int  -- ^ The bit (0-28) in the block version field used to signal for this feature (only if status is "started")
  , start_time  :: Int        -- ^ The minimum medium time
  , timeout     :: Int        -- ^ The minimum medium time at which point the deployment is considered failed
  , since       :: Int        -- ^ The height of the first block to which the status applies
  , statistics  :: Maybe Bip9Statistics
  } deriving (Eq, Show, Generic)

instance FromJSON Bip9Info

data SoftFork = SoftFork
  { forktype  :: Text       -- ^ "buried" or "bip9"
  , active    :: Bool       -- ^ True if the rules are enforced for the mempool and the next block
  , height    :: Maybe Int  -- ^ Height of the first block where the rules will be enforced (only if "buried" or "bip9" with active status)
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
  { chain                 :: Text       -- ^ "main", "test" or "regtest"
  , blocks                :: Int        -- ^ Height of the most fully validated chain
  , headers               :: Int        -- ^ Current number of headers validated
  , bestblockhash         :: Text       -- ^ Hash of the currently best block
  , difficulty            :: Float      -- ^ Current difficulty
  , mediantime            :: Int        -- ^ Median time for the current best block
  , verificationprogress  :: Float      -- ^ Estimate of verification progress (0 to 1)
  , initialblockdownload  :: Bool       -- ^ Estimate of whether this node is in initial block download mode
  , chainwork             :: Text       -- ^ Total amount of work in active chain, in hexadecimal
  , size_on_disk          :: Int        -- ^ Estimated size of the block and undo files on disk
  , pruned                :: Bool       -- ^ True if the block are subject to pruning
  , pruneheight           :: Maybe Int  -- ^ Lowest height of complete block stored (only if pruning enabled)
  , automatic_pruning     :: Maybe Bool -- ^ True if automatic pruning is enabled
  , softforks             :: Maybe (Map Text SoftFork)
  , warnings              :: Text       -- ^ Any network and blockchain warnings
  } deriving (Eq, Show, Generic)

instance FromJSON BlockchainInfo

-- | Get blockchain info
getBlockchainInfo :: MonadIO m => BitcoinRPCClient -> m BlockchainInfo
getBlockchainInfo client = do
  response <- callBitcoinRPC client "getblockchaininfo"
  let Just res = decode $ encode $ result $ getResponseBody response
  return res

