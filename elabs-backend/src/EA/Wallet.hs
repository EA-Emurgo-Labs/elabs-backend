{-# OPTIONS_GHC -Wno-deprecations #-}
module EA.Wallet (
  WalletId (..),
  eaGetAddress,
  eaSignGYTxBody,
) where

import qualified Data.Text.Class as TC
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Swagger as Swagger

import GeniusYield.Types (GYAddress, GYTx, GYTxBody)

import Servant
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece) )

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