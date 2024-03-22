{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Internal.Ipfs (
  ipfsPinnedObjects,
  ipfsAddFile,
  ipfsPinObject,
) where

import EA (
  EAApp,
  EAAppEnv (eaAppEnvBlockfrostIpfsProjectId),
  eaLiftEither,
 )
import Internal.Ipfs.Types (IpfsAddResponse, IpfsListResponse, IpfsPin)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (
  Capture,
  Get,
  Header,
  JSON,
  Post,
  (:<|>) ((:<|>)),
  type (:>),
 )
import Servant.Client (
  BaseUrl (BaseUrl),
  ClientError,
  ClientM,
  Scheme (Https),
  client,
  mkClientEnv,
  runClientM,
 )
import Servant.Multipart (
  FileData,
  MultipartData (MultipartData),
  MultipartForm,
  Tmp,
 )
import Servant.Multipart.Client ()

--------------------------------------------------------------------------------

type BlockfrostIpfsApi = IpfsList :<|> IpfsAddPin :<|> IpfsAdd

type IpfsList =
  "pin"
    :> "list"
    :> Header "project_id" String
    :> Get '[JSON] [IpfsListResponse]

type IpfsAddPin =
  "pin"
    :> "add"
    :> Header "project_id" String
    :> Capture "IpfsPath" Text
    :> Post '[JSON] IpfsPin

type IpfsAdd =
  "add"
    :> Header "project_id" String
    :> MultipartForm Tmp (MultipartData Tmp)
    :> Post '[JSON] IpfsAddResponse

api :: Proxy BlockfrostIpfsApi
api = Proxy

ipfsList :<|> ipfsPin :<|> ipfsAdd = client api

-- | Run the client
runClient' :: ClientM a -> IO (Either ClientError a)
runClient' client = do
  manager <- newManager tlsManagerSettings
  let
    env =
      mkClientEnv
        manager
        (BaseUrl Https "ipfs.blockfrost.io" 443 "api/v0/ipfs")
  runClientM client env

-- | List pinned objects
ipfsPinnedObjects :: EAApp [IpfsListResponse]
ipfsPinnedObjects = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show
    =<< (liftIO . runClient' $ ipfsList (Just token))

-- | Add a file to IPFS
ipfsAddFile :: FileData Tmp -> EAApp IpfsAddResponse
ipfsAddFile fileData = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show
    =<< ( liftIO . runClient' $
            ipfsAdd (Just token) ("----EalabsBackendBoundary", multipart)
        )
  where
    multipart = MultipartData [] [fileData]

-- | Pin an object
ipfsPinObject :: Text -> EAApp IpfsPin
ipfsPinObject path = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show
    =<< (liftIO . runClient' $ ipfsPin (Just token) path)
