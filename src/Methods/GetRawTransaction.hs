{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetRawTransaction
  ( getRawTransaction
  , Transaction (..)
  ) where

import RPC
import Types
import BitcoinRPCClient

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple (getResponseBody)

data Transaction = Transaction
  { in_active_chain   :: Bool
  , hex               :: Text
  , txid              :: TxID
  , hash              :: Text
  , size              :: Int
  , vsize             :: Int
  , weight            :: Int
  , version           :: Int
  , locktime          :: Int
  , blockhash         :: BlockHash
  , confirmations     :: Int
  , blocktime         :: Int
  , time              :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Transaction

getRawTransaction :: MonadIO m => BitcoinRPCClient -> TxID -> m (Either Text Transaction)
getRawTransaction client txid = callBitcoinRPC client "getrawtransaction" [txid, "true"]

