module EA.Api.Mint (MintApi, handleMintApi) where

import Data.Text qualified as T

import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.Imports
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYAddress,
  GYTxBody,
  GYTxOutRefCbor (getTxOutRefHex),
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
  txBodyFee,
  txToHex,
  unsignedTx,
 )

import Servant (JSON, Post, ReqBody, type (:>))

import Data.Maybe (fromJust)
import Data.Swagger qualified as Swagger

import EA (EAApp, EAAppEnv (..), oneShotMintingPolicy)
import EA.Tx.OneShotMint qualified as Tx

type MintApi =
  "one-shot-mint"
    :> ReqBody '[JSON] WalletParams
    :> Post '[JSON] UnsignedTxResponse

data WalletParams = WalletParams
  { usedAddrs :: ![GYAddress]
  , changeAddr :: !GYAddress
  , collateral :: !(Maybe GYTxOutRefCbor)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, Swagger.ToSchema)

data UnsignedTxResponse = UnsignedTxResponse
  { txBodyHex :: !T.Text
  , txFee :: !(Maybe Integer)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema)

unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody =
  UnsignedTxResponse
    { txBodyHex = T.pack $ txToHex $ unsignedTx txBody
    , txFee = Just $ txBodyFee txBody
    }

handleMintApi :: WalletParams -> EAApp UnsignedTxResponse
handleMintApi WalletParams {..} = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers usedAddrs
  oref <- liftIO $ fst . fromJust <$> randomTxOutRef utxos -- FIXME:
  policy <- asks (oneShotMintingPolicy oref)

  let
    addr = fromJust $ viaNonEmpty head usedAddrs -- FIXME:
    skeleton = Tx.oneShotMint addr oref 1 policy

  txBody <-
    liftIO
      $ runGYTxMonadNode
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
        (return skeleton)

  pure $ unSignedTxWithFee txBody
