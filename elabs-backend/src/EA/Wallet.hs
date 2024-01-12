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

import GeniusYield.Types (GYAddress, GYTx, GYTxBody, GYTxOutRef)

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
