module EA.Api.Carbon (
  CarbonApi,
  CarbonMintRequest (..),
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA.Api.Types (SubmitTxResponse)
import Servant (Header, JSON, Post, ReqBody, type (:>))
import Servant.Multipart (MultipartData, MultipartForm, Tmp)

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> Header "user_id" Text
    :> MultipartForm Tmp (MultipartData Tmp)
    :> ReqBody '[JSON] CarbonMintRequest
    :> "mint"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data CarbonMintRequest = CarbonMintRequest
  { amount :: !Int
  -- ^ The amount of carbon to mint.
  , sell :: !Int
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)
