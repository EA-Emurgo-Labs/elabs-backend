module EA.Api.Types (
  UserId (..),
  SubmitTxParams (..),
  SubmitTxResponse (..),
  WalletParams (..),
  UnsignedTxResponse (..),
  WalletResponse (..),
  txBodySubmitTxResponse,
  unSignedTxWithFee,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Class qualified as TC

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
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

import Database.Persist (
  PersistField (fromPersistValue),
  PersistValue,
  SqlType (SqlInt64),
 )
import Database.Persist.Class (PersistField (toPersistValue))
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.Sqlite (PersistFieldSql (sqlType))

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- UserId

newtype UserId = UserId {unUserId :: Natural}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToParamSchema)

instance FromHttpApiData UserId where
  parseUrlPiece = bimap (T.pack . TC.getTextDecodingError) UserId . TC.fromText

instance ToHttpApiData UserId where
  toUrlPiece = TC.toText . unUserId

-- Check OverflowNatural documentation
instance PersistField UserId where
  toPersistValue =
    (toPersistValue :: Int64 -> PersistValue) . fromIntegral . unUserId
  fromPersistValue x = case (fromPersistValue x :: Either Text Int64) of
    Left err -> Left $ T.replace "Int64" "UserId" err
    Right int -> Right $ UserId $ fromIntegral int

instance PersistFieldSql UserId where
  sqlType _ = SqlInt64

--------------------------------------------------------------------------------
-- Wallet

data WalletResponse = WalletResponse
  { addresses :: ![GYAddress]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)
