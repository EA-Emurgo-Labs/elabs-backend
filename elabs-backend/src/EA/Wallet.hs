{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  eaSignGYTxBody,
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
  eaCreateAddresses,
) where

import GeniusYield.Types (GYAddress, GYTx, GYTxBody, GYTxOutRef)

import Database.Persist.Sql (runSqlPool)

import EA (EAApp, eaAppEnvSqlPool, eaLiftMaybe)
import EA.Api.Types (UserId)

import Data.Aeson.Decoding qualified as Aeson
import Internal.Wallet.DB.Sqlite (getAddresses)

--------------------------------------------------------------------------------

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses = eaGetAddresses False

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses = eaGetAddresses True

eaGetAddresses :: Bool -> UserId -> EAApp [GYAddress]
eaGetAddresses used userid = do
  addresses <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getAddresses userid used)
          )
  mapM (eaLiftMaybe "Decoding error" . Aeson.decode . fromStrict) addresses

eaCreateAddresses :: UserId -> EAApp [GYAddress]
eaCreateAddresses = undefined

eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined
