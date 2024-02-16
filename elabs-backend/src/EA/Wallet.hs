module EA.Wallet
  ( eaGetCollateral,
    eaCreateAddresses,
    eaGetInternalAddresses,
    eaGetCollateralFromInternalWallet,
    eaGetAddresses,
    eaSelectOref,
  )
where

import Database.Persist.Sql (runSqlPool)
import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaGetCollateral, eaLiftEither)
import EA.Api.Types (UserId)
import GeniusYield.Types
  ( GYAddress,
    GYTxOutRef,
    GYUTxO (utxoRef),
    filterUTxOs,
    gyQueryUtxosAtAddresses,
    randomTxOutRef,
  )
import Internal.Wallet (PaymentKey, deriveAddress)
import Internal.Wallet.DB.Sqlite
  ( createWalletIndexPair,
    getInternalWalletIndexPairs',
    getWalletIndexPairs',
  )

--------------------------------------------------------------------------------

eaGetInternalAddresses :: EAApp [(GYAddress, PaymentKey)]
eaGetInternalAddresses = do
  nid <- asks eaAppEnvGYNetworkId
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
  nid <- asks eaAppEnvGYNetworkId
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

-- FIXME: Maybe Maybe??
eaGetCollateralFromInternalWallet ::
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
eaGetCollateralFromInternalWallet = eaGetInternalAddresses >>= getCollateral

-- FIXME: Maybe Maybe??
getCollateral ::
  [(GYAddress, PaymentKey)] ->
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
getCollateral [] = return Nothing
getCollateral ((addr, key) : pairs) = do
  eaGetCollateral addr 5 >>= \case
    Nothing -> getCollateral pairs
    Just (oref, _) -> return $ Just (Just (oref, True), key)

eaSelectOref ::
  [(GYAddress, PaymentKey)] ->
  (GYTxOutRef -> Bool) ->
  EAApp (Maybe (GYAddress, PaymentKey, GYTxOutRef))
eaSelectOref [] _ = return Nothing
eaSelectOref ((addr, key) : pairs) checkOref = do
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers [addr]
  moref <- liftIO $ randomTxOutRef $ filterUTxOs (checkOref . utxoRef) utxos
  case moref of
    Nothing -> eaSelectOref pairs checkOref
    Just (oref, _) -> return $ Just (addr, key, oref)
