{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  UserId (..),
  eaGetAddresses,
  eaSignGYTxBody,
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Class qualified as TC

import GeniusYield.Types (GYAddress, GYTx, GYTxBody, GYTxOutRef)

import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

import Database.Persist.Sqlite (
  PersistField (..),
  PersistFieldSql (sqlType),
  PersistValue,
  SqlType (SqlInt64),
 )

import EA (EAApp)

--------------------------------------------------------------------------------

eaGetAddresses :: UserId -> EAApp [GYAddress]
eaGetAddresses = undefined

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses = undefined

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses = undefined

eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined

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
  toPersistValue = (toPersistValue :: Int64 -> PersistValue) . fromIntegral . unUserId
  fromPersistValue x = case (fromPersistValue x :: Either Text Int64) of
    Left err -> Left $ T.replace "Int64" "UserId" err
    Right int -> Right $ UserId $ fromIntegral int

instance PersistFieldSql UserId where
  sqlType _ = SqlInt64
