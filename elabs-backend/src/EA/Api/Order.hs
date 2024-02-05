module EA.Api.Order (OrderApi) where

import Servant (JSON, type (:<|>), type (:>), Post, Capture, Header, ReqBody)
import qualified Data.Aeson as Aeson
import qualified Data.Swagger as Swagger
import EA.Api.Types (SubmitTxResponse)

--------------------------------------------------------------------------------

type OrderApi = OrderCreate :<|> OrderBuy :<|> OrderCancel :<|> OrderUpdate

type OrderCreate =
  "order"
    :> Header "user_id" Text
    :> ReqBody '[JSON] OrderRequest
    :> "create"
    :> Post '[JSON] SubmitTxResponse

type OrderBuy =
  "order"
    :> Header "user_id" Text
    :> ReqBody '[JSON] OrderBuyRequest
    :> Capture "id" Int
    :> "buy"
    :> Post '[JSON] SubmitTxResponse

type OrderCancel =
  "order"
    :> Header "user_id" Text
    :> Capture "id" Int
    :> "cancel"
    :> Post '[JSON] SubmitTxResponse

type OrderUpdate =
  "order"
    :> Header "user_id" Text
    :> ReqBody '[JSON] OrderRequest
    :> Capture "id" Int
    :> "update"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data OrderRequest = OrderRequest
  { amount :: !Int
  -- ^ The amount of carbon to mint.
  , sell :: !Int
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderBuyRequest = OrderBuyRequest
  { amount :: !Int
  -- ^ The amount of carbon to buy.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)