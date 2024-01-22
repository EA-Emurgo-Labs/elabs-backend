module EA.Wallet (
  eaGetCollateral,
  eaCreateAddresses,
  eaGetInternalAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetAddresses,
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
  getInternalWalletIndexPairs',
  getWalletIndexPairs',
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
                (getInternalWalletIndexPairs' 1)
          )
  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaGetAddresses :: UserId -> EAApp [(GYAddress, PaymentKey)]
eaGetAddresses userId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  rootK <- asks eaAppEnvRootKey
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getWalletIndexPairs' userId 5)
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
