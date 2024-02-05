{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Internal.Ipfs (
  ipfsPinnedObjects,
  ipfsAddFile,
  ipfsPinObject,
) where

import Control.Exception (ErrorCall (ErrorCall))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import EA (
  EAApp,
  EAAppEnv (eaAppEnvBlockfrostIpfsProjectId),
  eaLiftEither,
  eaThrow,
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
  FileData (FileData),
  MultipartData (MultipartData),
  MultipartForm,
  Tmp,
 )
import Servant.Multipart.Client ()
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)

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
ipfsAddFile :: FilePath -> EAApp IpfsAddResponse
ipfsAddFile fp = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  hasFile <- liftIO $ doesFileExist fp
  if hasFile
    then do
      _fileContents <- liftIO $ LBS.readFile fp
      let
        fn = T.pack $ takeBaseName fp
        multipart =
          MultipartData
            []
            [FileData "file" fn "application/octet-stream" fp]
      eaLiftEither show
        =<< ( liftIO . runClient' $
                ipfsAdd (Just token) ("----EalabsBackendBoundary", multipart)
            )
    else eaThrow . ErrorCall $ "File not found: " ++ fp

-- | Pin an object
ipfsPinObject :: Text -> EAApp IpfsPin
ipfsPinObject path = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show
    =<< (liftIO . runClient' $ ipfsPin (Just token) path)
