module EA.Api.Mint (
  MintApi,
  handleOneShotMintByWalletId,
  handleOneShotMintByWallet,
) where

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYProviders (gySubmitTx),
  GYTxOutRefCbor (getTxOutRefHex),
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
 )

import Servant (Capture, JSON, Post, ReqBody, (:<|>), type (:>))

import EA (EAApp, EAAppEnv (..), eaLiftMaybe, oneShotMintingPolicy)
import EA.Api.Types (
  SubmitTxResponse,
  UnsignedTxResponse,
  WalletParams (..),
  txBodySubmitTxResponse,
  unSignedTxWithFee,
 )
import EA.Tx.OneShotMint qualified as Tx
import EA.Wallet (WalletId, eaGetAddresses, eaGetCollateral, eaSignGYTxBody)

type MintApi = OneShotMintByWallet :<|> OneShotMintByWalletId

type OneShotMintByWallet =
  "one-shot-mint"
    :> ReqBody '[JSON] WalletParams
    :> Post '[JSON] UnsignedTxResponse

type OneShotMintByWalletId =
  "one-shot-mint"
    :> Capture "walletId" WalletId
    :> Post '[JSON] SubmitTxResponse

handleOneShotMintByWalletId :: WalletId -> EAApp SubmitTxResponse
handleOneShotMintByWalletId walletId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  addrs <- eaGetAddresses walletId
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers addrs

  (oref, _) <-
    liftIO (randomTxOutRef utxos) >>= eaLiftMaybe "No UTxO found"

  policy <- asks (oneShotMintingPolicy oref)

  addr <- eaLiftMaybe "No address provided" $ viaNonEmpty head addrs
  collateral <- eaGetCollateral

  txBody <-
    liftIO $
      runGYTxMonadNode
        nid
        providers
        [addr]
        addr
        collateral
        (return $ Tx.oneShotMint addr oref 1 policy)

  signedTx <- eaSignGYTxBody txBody

  void . liftIO $ gySubmitTx providers signedTx
  return $ txBodySubmitTxResponse txBody

handleOneShotMintByWallet :: WalletParams -> EAApp UnsignedTxResponse
handleOneShotMintByWallet WalletParams {..} = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers usedAddrs

  (oref, _) <-
    liftIO (randomTxOutRef utxos) >>= eaLiftMaybe "No UTxO found"

  policy <- asks (oneShotMintingPolicy oref)

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
                      ( getTxOutRefHex c
                      , True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                      )
                )
        )
        (return $ Tx.oneShotMint addr oref 1 policy)

  pure $ unSignedTxWithFee txBody
