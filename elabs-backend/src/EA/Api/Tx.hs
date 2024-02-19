module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA (EAApp, eaSubmitTx)
import EA.Api.Types (
  SubmitTxParams (..),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import GeniusYield.Types (
  getTxBody,
  makeSignedTransaction,
 )
import Servant (
  Capture,
  GenericMode ((:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
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
    :> Get '[JSON] TxStatusResponse

type TxSubmit =
  "tx"
    :> "submit"
    :> ReqBody '[JSON] SubmitTxParams
    :> Post '[JSON] SubmitTxResponse

-- TODO:
data TxStatusResponse = TxStatusResponse
  { txStatus :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleTxSubmit :: SubmitTxParams -> EAApp SubmitTxResponse
handleTxSubmit SubmitTxParams {..} = do
  void $ eaSubmitTx $ makeSignedTransaction txWit txBody
  return $ txBodySubmitTxResponse txBody
  where
    txBody = getTxBody txUnsigned

-- TODO:
handleTxStatus :: Text -> EAApp TxStatusResponse
handleTxStatus _ = error "TODO"
