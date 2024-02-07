module EA.Api.Carbon (
  CarbonApi,
  CarbonMintRequest (..),
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA.Api.Types (SubmitTxResponse, UserId)
import Servant (JSON, Post, type (:>))
import Servant.Multipart (MultipartData, MultipartForm, Tmp)

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> MultipartForm Tmp (MultipartData Tmp)
    :> "mint"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data CarbonMintRequest = CarbonMintRequest
  { userId :: !UserId
  -- ^ The user ID.
  , amount :: !Int
  -- ^ The amount of carbon to mint.
  , sell :: !Int
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)
