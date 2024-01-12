module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import GeniusYield.Types (
  GYProviders (gySubmitTx),
  getTxBody,
  makeSignedTransaction,
 )

import Servant (JSON, Post, ReqBody, type (:>))

import EA (EAApp, EAAppEnv (..))
import EA.Api.Types (
  SubmitTxParams (..),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )

type TxApi =
  "submit"
    :> ReqBody '[JSON] SubmitTxParams
    :> Post '[JSON] SubmitTxResponse

handleTxApi :: SubmitTxParams -> EA.EAApp SubmitTxResponse
handleTxApi SubmitTxParams {..} = do
  providers <- asks eaAppEnvGYProviders
  void . liftIO $ gySubmitTx providers $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned
