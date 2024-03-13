module EA.Wallet (
  eaGetCollateral,
  eaGetInternalAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetAddresses,
  eaSelectOref,
)
where

import Database.Persist.Sql (runSqlPool)
import EA (
  EAApp,
  EAAppEnv (..),
  eaAppEnvSqlPool,
  eaGetCollateral,
  eaLiftEither,
  eaLiftMaybe,
 )
import EA.Api.Types (UserId)
import GeniusYield.Types (
  GYAddress,
  GYTxOutRef,
  GYUTxO (utxoRef),
  addressToPubKeyHash,
  filterUTxOs,
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
 )
import Internal.Wallet (PaymentKey, deriveAddress)
import Internal.Wallet.DB.Sql (
  getInternalWalletIndexPairs',
  getWalletIndexPairs',
  saveToUserLookup,
 )

--------------------------------------------------------------------------------

eaGetInternalAddresses :: Bool -> EAApp [(GYAddress, PaymentKey)]
eaGetInternalAddresses collateral = do
  nid <- asks eaAppEnvGYNetworkId
  rootK <- asks eaAppEnvRootKey
  indexPairs <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getInternalWalletIndexPairs' 1 collateral)
          )
  -- \^ Need to be 1 because how ChangeBlock smart contract v1 is implemented
  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaGetAddresses :: UserId -> EAApp [(GYAddress, PaymentKey)]
eaGetAddresses userId = do
  nid <- asks eaAppEnvGYNetworkId
  rootK <- asks eaAppEnvRootKey
  pool <- asks eaAppEnvSqlPool
  indexPairs <-
    liftIO $ runSqlPool (getWalletIndexPairs' userId 1) pool
  -- \^ Need to be 1 because how ChangeBlock smart contract v1 is implemented

  -- Save derived pub key hash
  pairs <-
    eaLiftEither id $
      mapM (uncurry $ deriveAddress nid rootK) indexPairs
  pkh <- eaLiftMaybe "cannot get pub key hash from address" $ do
    (addr, _) <- listToMaybe pairs
    addressToPubKeyHash addr
  void . liftIO $ runSqlPool (saveToUserLookup userId pkh) pool
  return pairs

-- FIXME: Maybe Maybe??
eaGetCollateralFromInternalWallet ::
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
eaGetCollateralFromInternalWallet = eaGetInternalAddresses True >>= getCollateral

-- FIXME: Maybe Maybe??
getCollateral ::
  [(GYAddress, PaymentKey)] ->
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
getCollateral [] = return Nothing
getCollateral ((addr, key) : pairs) = do
  eaGetCollateral addr 5_000_000 >>= \case
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
