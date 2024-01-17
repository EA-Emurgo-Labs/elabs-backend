{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  eaSignGYTxBody,
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
  eaCreateAddresses,
) where

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.Types (
  GYAddress,
  GYTx,
  GYTxBody,
  GYTxOutRef,
 )

import Database.Persist.Sql (runSqlPool)

import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaLiftEither)
import EA.Api.Types (UserId)

import Internal.Wallet (deriveAddress)
import Internal.Wallet.DB.Sqlite (
  createWalletIndexPair,
  getUnusedWalletIndexPairs,
  getWalletIndexPairs,
 )

--------------------------------------------------------------------------------

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses userId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  rootK <- asks eaAppEnvRootKey
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getUnusedWalletIndexPairs userId 5)
          )
  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses = eaGetAddresses True

eaGetAddresses :: Bool -> UserId -> EAApp [GYAddress]
eaGetAddresses used userId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  rootK <- asks eaAppEnvRootKey
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getWalletIndexPairs userId used)
          )
  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaCreateAddresses :: UserId -> Int -> EAApp ()
eaCreateAddresses userId n = do
  asks eaAppEnvSqlPool
    >>= ( liftIO
            . runSqlPool
              (createWalletIndexPair userId n)
        )

eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined
