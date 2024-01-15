{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  eaGetAddresses,
  eaSignGYTxBody,
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
) where

import GeniusYield.Types (GYAddress, GYTx, GYTxBody, GYTxOutRef)

import Database.Persist.Sql (runSqlPool)

import EA (EAApp, eaAppEnvSqlPool)
import EA.Api.Types (UserId)

import Internal.Wallet.DB.Sqlite (getAddresses)

--------------------------------------------------------------------------------

eaGetAddresses :: UserId -> EAApp [GYAddress]
eaGetAddresses = undefined

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses userid =
  asks eaAppEnvSqlPool
    >>= ( liftIO
            . runSqlPool
              (getAddresses userid False)
        )

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses userid =
  asks eaAppEnvSqlPool
    >>= ( liftIO
            . runSqlPool
              (getAddresses userid True)
        )

eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined
