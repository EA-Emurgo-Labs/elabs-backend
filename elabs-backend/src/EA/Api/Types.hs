{-# OPTIONS_GHC -Wno-orphans #-}

module EA.Api.Types (
  SubmitTxParams (..),
  SubmitTxResponse (..),
  WalletParams (..),
  UnsignedTxResponse (..),
  txBodySubmitTxResponse,
  unSignedTxWithFee,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Class qualified as TC

import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

import GeniusYield.Types (
  GYAddress,
  GYTx,
  GYTxBody,
  GYTxId,
  GYTxOutRefCbor,
  GYTxWitness,
  txBodyFee,
  txBodyTxId,
  txToHex,
  unsignedTx,
 )

import EA.Wallet (UserId (..))

data SubmitTxParams = SubmitTxParams
  { txUnsigned :: !GYTx
  , txWit :: !GYTxWitness
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data SubmitTxResponse = SubmitTxResponse
  { submitTxFee :: !Integer
  , submitTxId :: !GYTxId
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody =
  SubmitTxResponse
    { submitTxFee = txBodyFee txBody
    , submitTxId = txBodyTxId txBody
    }

data WalletParams = WalletParams
  { usedAddrs :: ![GYAddress]
  , changeAddr :: !GYAddress
  , collateral :: !(Maybe GYTxOutRefCbor)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data UnsignedTxResponse = UnsignedTxResponse
  { txBodyHex :: !T.Text
  , txFee :: !(Maybe Integer)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody =
  UnsignedTxResponse
    { txBodyHex = T.pack $ txToHex $ unsignedTx txBody
    , txFee = Just $ txBodyFee txBody
    }

instance FromHttpApiData UserId where
  parseUrlPiece = bimap (T.pack . TC.getTextDecodingError) UserId . TC.fromText

instance ToHttpApiData UserId where
  toUrlPiece = TC.toText . unUserId
