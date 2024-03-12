{-# LANGUAGE NoOverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EA.Orphans (
  MultipartFormDataTmp,
  GYPubKeyHash,
) where

import Cardano.Api qualified as Api
import Control.Lens ((%~), (.~), (?~))
import Data.Aeson
import Data.Swagger (
  MimeList (MimeList),
  Param,
  ParamAnySchema (ParamOther),
  ParamLocation (ParamFormData),
  Referenced (Inline),
  Swagger,
  SwaggerType (..),
  allOperations,
  toParamSchema,
 )
import Data.Swagger.Lens
import Data.Text qualified as T
import Database.Persist (PersistField, PersistValue, SqlType (SqlString))
import Database.Persist.Class.PersistField (PersistField (fromPersistValue))
import Database.Persist.Postgresql (PersistFieldSql (sqlType))
import Database.Persist.Sql (PersistField (toPersistValue))
import EA.Api.Types (CarbonMintRequest (..), UserId (..))
import GeniusYield.Types (GYPubKeyHash, pubKeyHashToApi)
import Network.HTTP.Media (MediaType)
import Servant (Tagged, (:>))
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Swagger (HasSwagger (..))

--------------------------------------------------------------------------------

type MultipartFormDataTmp = MultipartForm Tmp (MultipartData Tmp)

-- | Add parameter to every operation in the spec.
addParam :: Param -> Swagger -> Swagger
addParam param = allOperations . parameters %~ (Inline param :)

-- | Add accepted content types to every operation in the spec.
addConsumes :: [MediaType] -> Swagger -> Swagger
addConsumes cs = allOperations . consumes %~ (<> Just (MimeList cs))

instance
  (HasSwagger api) =>
  HasSwagger (Tagged CarbonMintRequest MultipartFormDataTmp :> api)
  where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & addConsumes ["multipart/form-data"]
      & addParam fileParam
      & addParam jsonParam
    where
      fileParam =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema
            .~ ParamOther
              ( mempty
                  & in_ .~ ParamFormData
                  & paramSchema .~ (mempty & type_ ?~ SwaggerFile)
              )

      jsonParam =
        mempty
          & name .~ "data"
          & required ?~ True
          & description ?~ "JSON data"
          & schema
            .~ ParamOther
              ( mempty
                  & in_ .~ ParamFormData
                  & allowEmptyValue ?~ False
                  & paramSchema
                    .~ ( toParamSchema (Proxy :: Proxy String)
                          & default_ ?~ toJSON carbonMintRequestExample
                       )
              )

carbonMintRequestExample :: CarbonMintRequest
carbonMintRequestExample = CarbonMintRequest (UserId 14) 100000 1000

--------------------------------------------------------------------------------

instance PersistField GYPubKeyHash where
  toPersistValue =
    (toPersistValue :: String -> PersistValue)
      . T.unpack
      . Api.serialiseToRawBytesHexText
      . pubKeyHashToApi

  fromPersistValue x = case (fromPersistValue x :: Either Text String) of
    Left err -> Left $ T.replace "String" "GYPubKeyHash" err
    Right pkh -> Right $ fromString pkh

instance PersistFieldSql GYPubKeyHash where
  sqlType _ = SqlString
