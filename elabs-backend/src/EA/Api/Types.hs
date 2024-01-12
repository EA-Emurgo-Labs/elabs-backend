module EA.Api.Types (
 SubmitTxParams (..),
 SubmitTxResponse (..),
 WalletParams (..),
 UnsignedTxResponse (..),
 txBodySubmitTxResponse,
 unSignedTxWithFee
) where

import GeniusYield.Types (
  GYTx,
  GYTxWitness, GYTxId, GYTxBody, txBodyFee, txBodyTxId, GYAddress, GYTxOutRefCbor, unsignedTx, txToHex)
import qualified Data.Swagger as Swagger
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

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