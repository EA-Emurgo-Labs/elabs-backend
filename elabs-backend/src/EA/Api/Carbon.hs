module EA.Api.Carbon (
  CarbonApi,
  CarbonMintRequest (..),
  handleCarbonMint,
) where

import Control.Exception (ErrorCall (ErrorCall))
import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA (EAApp, eaLiftEither, eaLiftMaybe, eaLogInfo, eaThrow)
import EA.Api.Types (UserId)
import Internal.Ipfs (ipfsAddFile, ipfsPinObject)
import Internal.Ipfs.Types (IpfsAddResponse (..), IpfsPin (..))
import Servant (Header, JSON, Post, type (:>))
import Servant.Multipart (
  MultipartData,
  MultipartForm,
  Tmp,
  lookupFile,
  lookupInput,
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> Header "user_id" UserId
    :> MultipartForm Tmp (MultipartData Tmp)
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------
-- FIXME: This is because of MultiparForm, which is not supported by HasSwagger

type CarbonMintFix =
  "carbon"
    :> Header "user_id" UserId
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

instance {-# OVERLAPPING #-} HasSwagger CarbonApi where
  toSwagger _ = toSwagger (Proxy :: Proxy CarbonMintFix)

--------------------------------------------------------------------------------

data CarbonMintRequest = CarbonMintRequest
  { amount :: !Int
  -- ^ The amount of carbon to mint.
  , sell :: !Int
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data CarbonMintResponse = CarbonMintResponse
  { ipfsHash :: !Text
  , ipfsName :: !Text
  , ipfsSize :: !Text
  , ipfsPinningState :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

-- TODO:
-- { submitTxFee :: !Integer
-- , submitTxId :: !GYTxId
-- }

--------------------------------------------------------------------------------

handleCarbonMint ::
  Maybe UserId ->
  MultipartData Tmp ->
  EAApp CarbonMintResponse
handleCarbonMint Nothing _ = eaThrow . ErrorCall $ "No UserId found in header"
handleCarbonMint (Just _usedId) multipartData = do
  filePart <- eaLiftEither id $ lookupFile "file" multipartData
  dataPart <- eaLiftEither id $ lookupInput "data" multipartData

  request <-
    eaLiftMaybe "Cannot decode JSON data" $
      Aeson.decode @CarbonMintRequest $
        encodeUtf8 dataPart

  ipfsAddResp <- ipfsAddFile filePart
  ipfsPinObjResp <- ipfsPinObject ipfsAddResp.ipfs_hash

  eaLogInfo "carbon-mint" $ show request

  -- TODO: mint NFT + mint carbon + create order tx

  return $
    CarbonMintResponse
      ipfsAddResp.ipfs_hash
      ipfsAddResp.name
      ipfsAddResp.size
      ipfsPinObjResp.state
