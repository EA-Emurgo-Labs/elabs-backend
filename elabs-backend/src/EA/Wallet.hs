module EA.Wallet (
  eaGetCollateral,
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
import GeniusYield.Types (
  GYAddress,
  GYProviders,
  GYTxOutRef,
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
 )
import Internal.Wallet (PaymentKey, deriveAddress)
import Internal.Wallet.DB.Sqlite (
  createWalletIndexPair,
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

eaSelectOref ::
  GYProviders ->
  [(GYAddress, PaymentKey)] ->
  IO (Maybe (GYAddress, PaymentKey, GYTxOutRef))
eaSelectOref _ [] = return Nothing
eaSelectOref providers ((addr, key) : pairs) = do
  utxos <- gyQueryUtxosAtAddresses providers [addr]
  moref <- randomTxOutRef utxos
  case moref of
    Nothing -> eaSelectOref providers pairs
    Just (oref, _) -> return $ Just (addr, key, oref)
