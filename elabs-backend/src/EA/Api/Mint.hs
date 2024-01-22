module EA.Api.Mint
  ( MintApi,
    handleOneShotMintByUserId,
    handleOneShotMintByWallet,
  )
where

import EA (EAApp, EAAppEnv (..), eaLiftMaybe, eaSubmitTx, oneShotMintingPolicy)
import EA.Api.Types
  ( SubmitTxResponse,
    UnsignedTxResponse,
    UserId,
    WalletParams (..),
    txBodySubmitTxResponse,
    unSignedTxWithFee,
  )
import EA.Tx.OneShotMint qualified as Tx
import EA.Wallet
  ( eaGetCollateralFromInternalWallet,
    eaGetUnusedAddresses,
  )
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types
  ( GYTxOutRefCbor (getTxOutRefHex),
    gyQueryUtxosAtAddresses,
    randomTxOutRef,
  )
import Internal.Wallet (eaSignTx)
import Servant (Capture, JSON, Post, ReqBody, (:<|>), type (:>))

type MintApi = OneShotMintByWallet :<|> OneShotMintByUserId

type OneShotMintByWallet =
  "one-shot-mint"
    :> ReqBody '[JSON] WalletParams
    :> Post '[JSON] UnsignedTxResponse

type OneShotMintByUserId =
  "one-shot-mint"
    :> Capture "user" UserId
    :> Post '[JSON] SubmitTxResponse

handleOneShotMintByUserId :: UserId -> EAApp SubmitTxResponse
handleOneShotMintByUserId userId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts
  (addrs, keys) <- unzip <$> eaGetUnusedAddresses userId

  utxos <- liftIO $ gyQueryUtxosAtAddresses providers addrs

  (oref, _) <-
    liftIO (randomTxOutRef utxos) >>= eaLiftMaybe "No UTxO found"

  let policy = oneShotMintingPolicy oref scripts

  addr <- eaLiftMaybe "No address provided" $ viaNonEmpty head addrs

  eaGetCollateralFromInternalWallet >>= \case
    Nothing -> eaLiftMaybe "No collateral found" Nothing
    Just (collateral, key) -> do
      txBody <-
        liftIO $
          runGYTxMonadNode
            nid
            providers
            addrs
            addr
            collateral
            (return $ Tx.oneShotMint addr oref 1 policy)
      void $ eaSubmitTx $ eaSignTx txBody (key : keys)
      return $ txBodySubmitTxResponse txBody

handleOneShotMintByWallet :: WalletParams -> EAApp UnsignedTxResponse
handleOneShotMintByWallet WalletParams {..} = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers usedAddrs

  (oref, _) <-
    liftIO (randomTxOutRef utxos) >>= eaLiftMaybe "No UTxO found"

  let policy = oneShotMintingPolicy oref scripts

  addr <- eaLiftMaybe "No address provided" $ viaNonEmpty head usedAddrs

  txBody <-
    liftIO $
      runGYTxMonadNode
        nid
        providers
        [addr]
        addr
        ( collateral
            >>= ( \c ->
                    Just
                      (getTxOutRefHex c, True)
                )
        )
        (return $ Tx.oneShotMint addr oref 1 policy)

  pure $ unSignedTxWithFee txBody
