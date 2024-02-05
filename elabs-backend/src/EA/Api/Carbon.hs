module EA.Api.Carbon (
  CarbonApi,
  CarbonMintRequest (..),
) where

import Servant (JSON, type (:>), Post, Header, ReqBody)
import qualified Data.Aeson as Aeson
import qualified Data.Swagger as Swagger
import Servant.Multipart (MultipartForm, Tmp, MultipartData)
import EA.Api.Types (SubmitTxResponse)

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
