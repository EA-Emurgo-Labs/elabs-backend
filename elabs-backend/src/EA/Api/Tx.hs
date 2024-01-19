module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import GeniusYield.Types (
  getTxBody,
  makeSignedTransaction,
 )

import Servant (JSON, Post, ReqBody, type (:>))

import EA (EAApp, eaSubmitTx)
import EA.Api.Types (
  SubmitTxParams (..),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )

type TxApi =
  "submit"
    :> ReqBody '[JSON] SubmitTxParams
    :> Post '[JSON] SubmitTxResponse

handleTxApi :: SubmitTxParams -> EAApp SubmitTxResponse
handleTxApi SubmitTxParams {..} = do
  void $ eaSubmitTx $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned
