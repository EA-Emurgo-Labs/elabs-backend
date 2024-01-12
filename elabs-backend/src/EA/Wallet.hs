{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  WalletId (..),
  eaGetAddress,
  eaSignGYTxBody,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Class qualified as TC

import GeniusYield.Types (GYAddress, GYTx, GYTxBody)

import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

import EA (EAApp)

--------------------------------------------------------------------------------

eaGetAddress :: WalletId -> EAApp GYAddress
eaGetAddress = undefined

eaSignGYTxBody :: GYTxBody -> GYTx
eaSignGYTxBody = undefined

--------------------------------------------------------------------------------
-- WalletId

newtype WalletId = WalletId {unWalletId :: Natural}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToParamSchema)

instance FromHttpApiData WalletId where
  parseUrlPiece = bimap (T.pack . TC.getTextDecodingError) WalletId . TC.fromText

instance ToHttpApiData WalletId where
  toUrlPiece = TC.toText . unWalletId
