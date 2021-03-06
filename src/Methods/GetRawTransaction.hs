{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Methods.GetRawTransaction
  ( getRawTransaction
  , Transaction (..)
  , VIn (..)
  , VOut (..)
  , ScriptSig (..)
  , ScriptPubKey (..)
  ) where

import RPC
import Types
import BitcoinRPCClient

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Simple (getResponseBody)

data ScriptSig = ScriptSig
  { asm   :: Text
  , hex   :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ScriptSig

data VIn = VIn
  { txid          :: Maybe TxID
  , vout          :: Maybe Int
  , scriptSig     :: Maybe ScriptSig
  , sequence      :: Maybe Int
  , txinwitness   :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON VIn where
  parseJSON (Object o) = VIn
    <$> o .:? "txid"
    <*> o .:? "vout"
    <*> o .:? "scriptSig"
    <*> o .:? "sequence"
    <*> o .:? "txinwitness" .!= []
  parseJSON invalid = undefined

data ScriptPubKey = ScriptPubKey
  { asm         :: Text
  , hex         :: Text
  , reqSigs     :: Maybe Int
  , scripttype  :: Text
  , addresses   :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON ScriptPubKey where
  parseJSON (Object o) = ScriptPubKey
    <$> o .:  "asm"
    <*> o .:  "hex"
    <*> o .:? "reqSigs"
    <*> o .:  "type"
    <*> o .:? "addresses" .!= []
  parseJSON invalid = undefined

data VOut = VOut
  { value         :: Float
  -- , n             :: Int
  , scriptPubKey  :: ScriptPubKey
  } deriving (Eq, Show, Generic)

instance FromJSON VOut

data Transaction = Transaction
  { hex               :: Text
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
  , vin               :: [VIn]
  , vout              :: [VOut]
  } deriving (Eq, Show, Generic)

instance FromJSON Transaction

getRawTransaction :: MonadIO m => BitcoinRPCClient -> TxID -> m (Either Text Transaction)
getRawTransaction client txid = callBitcoinRPC client "getrawtransaction" [String txid, Bool True]

