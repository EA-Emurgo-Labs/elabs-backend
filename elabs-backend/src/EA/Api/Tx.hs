module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import GeniusYield.Imports (FromJSON, ToJSON)
import GeniusYield.Types (
  GYProviders (gySubmitTx),
  GYTx,
  GYTxBody,
  GYTxId,
  GYTxWitness,
  getTxBody,
  makeSignedTransaction,
  txBodyFee,
  txBodyTxId,
 )

import Servant (JSON, Post, ReqBody, type (:>))

import Data.Swagger qualified as Swagger

import EA (EAApp, EAAppEnv (..))

type TxApi =
  "submit"
    :> ReqBody '[JSON] SubmitTxParams
    :> Post '[JSON] SubmitTxResponse

data SubmitTxParams = SubmitTxParams
  { txUnsigned :: !GYTx
  , txWit :: !GYTxWitness
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, Swagger.ToSchema)

data SubmitTxResponse = SubmitTxResponse
  { submitTxFee :: !Integer
  , submitTxId :: !GYTxId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, Swagger.ToSchema)

txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody =
  SubmitTxResponse
    { submitTxFee = txBodyFee txBody
    , submitTxId = txBodyTxId txBody
    }

handleTxApi :: SubmitTxParams -> EA.EAApp SubmitTxResponse
handleTxApi SubmitTxParams {..} = do
  providers <- asks eaAppEnvGYProviders
  void . liftIO $ gySubmitTx providers $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned
