{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module EA.Orphans (
  MultipartFormDataTmp,
) where

import Servant.Multipart ( MultipartForm, Tmp, MultipartData )
import Servant.Swagger ( HasSwagger(..) )
import Servant ((:>), Tagged)
import Data.Swagger
    ( Referenced(Inline),
      allOperations,
      Param,
      Swagger, 
      ParamAnySchema (ParamOther), ParamLocation (ParamFormData), SwaggerType (..), MimeList (MimeList), toParamSchema)
import Control.Lens ( (%~), (.~), (?~) )
import Data.Swagger.Lens
import Network.HTTP.Media (MediaType)
import Data.Aeson
import EA.Api.Types (CarbonMintRequest(..), UserId (..))

--------------------------------------------------------------------------------

type MultipartFormDataTmp = MultipartForm Tmp (MultipartData Tmp)

-- | Add parameter to every operation in the spec.
addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

-- | Add accepted content types to every operation in the spec.
addConsumes :: [MediaType] -> Swagger -> Swagger
addConsumes cs = allOperations.consumes %~ (<> Just (MimeList cs))

instance HasSwagger api  => HasSwagger (Tagged CarbonMintRequest MultipartFormDataTmp :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & addConsumes ["multipart/form-data"]
    & addParam fileParam
    & addParam jsonParam
    where
      fileParam = mempty
        & name .~ "file"
        & required ?~ True
        & description ?~ "File to upload"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ SwaggerFile))

      jsonParam = mempty
        & name .~ "data"
        & required ?~ True
        & description ?~ "JSON data"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & allowEmptyValue ?~ False
            & paramSchema .~ (toParamSchema (Proxy :: Proxy String)
            & default_ ?~ toJSON carbonMintRequestExample))

carbonMintRequestExample :: CarbonMintRequest
carbonMintRequestExample = CarbonMintRequest (UserId 14) 100000 1000