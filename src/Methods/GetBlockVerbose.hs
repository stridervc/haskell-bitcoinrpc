{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetBlockVerbose
  ( getBlockVerbose
  , VerboseBlock (..)
  , BlockTransaction (..)
  ) where

import Types
import BitcoinRPCClient
import Methods.GetRawTransaction (VIn(..), VOut(..), ScriptSig(..), ScriptPubKey(..))

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)

-- | Transaction as returned inline in getblock verbosity 2.
-- Unlike Transaction from getrawtransaction, this does NOT have
-- blockhash, confirmations, blocktime, or time fields.
data BlockTransaction = BlockTransaction
  { hex               :: Text
  , txid              :: TxID
  , hash              :: Text
  , size              :: Int
  , vsize             :: Int
  , weight            :: Int
  , version           :: Int
  , locktime          :: Int
  , vin               :: [VIn]
  , vout              :: [VOut]
  } deriving (Eq, Show, Generic)

instance FromJSON BlockTransaction

-- | Block with full transaction data (getblock verbosity 2)
data VerboseBlock = VerboseBlock
  { hash              :: BlockHash
  , confirmations     :: Integer
  , size              :: Int
  , strippedsize      :: Int
  , weight            :: Int
  , height            :: Integer
  , version           :: Int
  , versionHex        :: Text
  , merkleroot        :: Text
  , tx                :: [BlockTransaction]
  , time              :: Int
  , mediantime        :: Int
  , nonce             :: Int
  , bits              :: Text
  , difficulty        :: Float
  , chainwork         :: Text
  , nTx               :: Int
  , previousblockhash :: Maybe BlockHash
  , nextblockhash     :: Maybe BlockHash
  } deriving (Eq, Show, Generic)

instance FromJSON VerboseBlock

-- | Get block with full transaction data (verbosity 2)
getBlockVerbose :: MonadIO m => BitcoinRPCClient -> BlockHash -> m (Either Text VerboseBlock)
getBlockVerbose client bhash = callBitcoinRPC client "getblock" [String bhash, Number 2]
