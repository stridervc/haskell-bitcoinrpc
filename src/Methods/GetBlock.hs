{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetBlock
  ( getBlock
  , BlockHash
  , TxID
  , Block (..)
  ) where

import RPC
import Types
import BitcoinRPCClient

import Data.Aeson
import GHC.Generics
import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple (getResponseBody)

data Block = Block
  { hash              :: BlockHash
  , confirmations     :: Integer
  , size              :: Int
  , strippedsize      :: Int
  , weight            :: Int
  , height            :: Integer
  , version           :: Int
  , versionHex        :: Text
  , merkleroot        :: Text
  , tx                :: [TxID]
  , time              :: Int
  , mediantime        :: Int
  , nonce             :: Int
  , bits              :: Text
  , difficulty        :: Float
  , chainwork         :: Text
  , nTx               :: Int
  , previousblockhash :: BlockHash
  , nextblockhash     :: Maybe BlockHash
  } deriving (Eq, Show, Generic)

instance FromJSON Block

-- | Get block info
getBlock :: MonadIO m => BitcoinRPCClient -> BlockHash -> m (Either Text Block)
getBlock client bhash = callBitcoinRPC client "getblock" [bhash]
