module EA.Api.Mint (
  MintApi,
  handleOneShotMintByWalletId,
  handleOneShotMintByWallet,
) where

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYTxOutRefCbor (getTxOutRefHex),
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
 )

import Servant (Capture, JSON, Post, ReqBody, (:<|>), type (:>))

import EA (EAApp, EAAppEnv (..), eaLiftMaybe, oneShotMintingPolicy)
import EA.Api.Types (
  UnsignedTxResponse,
  WalletId,
  WalletParams (..),
  unSignedTxWithFee,
 )
import EA.Tx.OneShotMint qualified as Tx

type MintApi = OneShotMintByWallet :<|> OneShotMintByWalletId

type OneShotMintByWallet =
  "one-shot-mint"
    :> ReqBody '[JSON] WalletParams
    :> Post '[JSON] UnsignedTxResponse

type OneShotMintByWalletId =
  "one-shot-mint"
    :> Capture "walletId" WalletId
    :> Post '[JSON] UnsignedTxResponse

handleOneShotMintByWalletId :: WalletId -> EAApp UnsignedTxResponse
handleOneShotMintByWalletId = undefined

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
