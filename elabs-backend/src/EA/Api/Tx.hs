module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import EA (EAApp, eaAppEnvGYProviders, eaCatch, eaLiftMaybe, eaSubmitTx)
import EA.Api.Types (
  SubmitTxParams (..),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import GeniusYield.Types (
  GYAwaitTxException (GYAwaitTxException),
  GYAwaitTxParameters (GYAwaitTxParameters),
  GYProviders (gyAwaitTxConfirmed),
  GYTxId,
  getTxBody,
  makeSignedTransaction,
  txIdFromHex,
 )
import Servant (
  Capture,
  GenericMode ((:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
  QueryParam,
  ReqBody,
  ToServantApi,
  type (:>),
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

data TxApi mode = TxApi
  { txStatus :: mode :- TxStatus
  , txSubmit :: mode :- TxSubmit
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes TxApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi TxApi))

handleTxApi :: ServerT (NamedRoutes TxApi) EAApp
handleTxApi =
  TxApi
    { txStatus = handleTxStatus
    , txSubmit = handleTxSubmit
    }

type TxStatus =
  "tx"
    :> Capture "id" Text
    :> "status"
    :> QueryParam "confirmation" Word64
    :> Get '[JSON] TxStatusResponse

type TxSubmit =
  "tx"
    :> "submit"
    :> ReqBody '[JSON] SubmitTxParams
    :> Post '[JSON] SubmitTxResponse

data TxStatusResponse = TxStatusResponse
  { txStatus :: TxStatusCode
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

data TxStatusCode = TX_CONFIRMED | TX_PENDING | TX_FAILED
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleTxSubmit :: SubmitTxParams -> EAApp SubmitTxResponse
handleTxSubmit SubmitTxParams {..} = do
  void $ eaSubmitTx $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned

handleTxStatus :: Text -> Maybe Word64 -> EAApp TxStatusResponse
handleTxStatus text confirmation = do
  txid <- eaLiftMaybe "Wrong transaction id" . txIdFromHex . T.unpack $ text
  eaCatch
    (getTxStatus txid (fromMaybe 3 confirmation))
    ( \(GYAwaitTxException _) -> return $ TxStatusResponse TX_PENDING
    )

getTxStatus :: GYTxId -> Word64 -> EAApp TxStatusResponse
getTxStatus txid n = do
  awaitTx <- asks (gyAwaitTxConfirmed . eaAppEnvGYProviders)
  liftIO $
    awaitTx
      (GYAwaitTxParameters 1 0 n)
      txid
  return $ TxStatusResponse TX_CONFIRMED
