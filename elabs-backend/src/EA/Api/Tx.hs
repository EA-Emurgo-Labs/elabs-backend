module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import GeniusYield.Imports
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

import Servant

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
  deriving anyclass (FromJSON)

data SubmitTxResponse = SubmitTxResponse
  { submitTxFee :: !Integer
  , submitTxId :: !GYTxId
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody =
  SubmitTxResponse
    { submitTxFee = txBodyFee txBody
    , submitTxId = txBodyTxId txBody
    }

handleTxApi :: SubmitTxParams -> EAApp SubmitTxResponse
handleTxApi SubmitTxParams {..} = do
  providers <- asks eaAppEnvGYProviders
  void . liftIO $ gySubmitTx providers $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned
