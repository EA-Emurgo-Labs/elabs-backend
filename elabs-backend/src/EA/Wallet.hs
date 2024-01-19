module EA.Wallet (
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
  eaCreateAddresses,
  eaGetInternalAddresses,
  eaGetCollateralFromInternalWallet,
) where

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.Types (
  GYAddress,
  GYTxOutRef,
 )

import Database.Persist.Sql (runSqlPool)

import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaGetCollateral, eaLiftEither)
import EA.Api.Types (UserId)

import Internal.Wallet (PaymentKey, deriveAddress)
import Internal.Wallet.DB.Sqlite (
  createWalletIndexPair,
  getInternalWalletIndexPairs,
  getUnusedWalletIndexPairs,
  getWalletIndexPairs,
 )

--------------------------------------------------------------------------------

eaGetInternalAddresses :: EAApp [(GYAddress, PaymentKey)]
eaGetInternalAddresses = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  rootK <- asks eaAppEnvRootKey
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                getInternalWalletIndexPairs
          )
  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaGetUnusedAddresses :: UserId -> EAApp [(GYAddress, PaymentKey)]
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

eaGetUsedAddresses :: UserId -> EAApp [(GYAddress, PaymentKey)]
eaGetUsedAddresses = eaGetAddresses True

eaGetAddresses :: Bool -> UserId -> EAApp [(GYAddress, PaymentKey)]
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
              (createWalletIndexPair (Just userId) n)
        )

eaGetCollateralFromInternalWallet ::
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
eaGetCollateralFromInternalWallet = eaGetInternalAddresses >>= getCollateral

getCollateral ::
  [(GYAddress, PaymentKey)] ->
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
getCollateral [] = return Nothing
getCollateral ((addr, key) : pairs) = do
  eaGetCollateral addr 5 >>= \case
    Nothing -> getCollateral pairs
    Just (oref, _) -> return $ Just (Just (oref, True), key)
