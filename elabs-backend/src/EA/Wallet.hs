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

import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaLiftMaybe, unRootKey)
import EA.Api.Types (UserId)

import Internal.Wallet (deriveAddress)
import Internal.Wallet.DB.Sqlite (createWalletIndexPair, getWalletIndexPairs)

--------------------------------------------------------------------------------

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses = eaGetAddresses False

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses = eaGetAddresses True

eaGetAddresses :: Bool -> UserId -> EAApp [GYAddress]
eaGetAddresses used userid = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  rootK <- asks (unRootKey . eaAppEnvRootKey)
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getWalletIndexPairs userid used)
          )
  eaLiftMaybe "Something went wrong with the address derivation" $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaCreateAddresses :: UserId -> EAApp [GYAddress]
eaCreateAddresses userid = do
  asks eaAppEnvSqlPool
    >>= ( liftIO
            . runSqlPool
              (createWalletIndexPair userid)
        )
  -- TODO: dont query again
  eaGetUnusedAddresses userid

eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined
