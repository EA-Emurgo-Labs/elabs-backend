{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Internal.Ipfs (
  ipfsPinnedObjects
, ipfsAddFile
, ipfsPinObject
) where


import Servant ( JSON, type (:>), Header, Get, Post, (:<|>) ((:<|>)), Capture)
import Internal.Ipfs.Types (IpfsListResponse, IpfsAddResponse, IpfsPin)
import Servant.Client (ClientM, client, mkClientEnv, BaseUrl (BaseUrl), Scheme (Https), runClientM, ClientError)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import EA (EAApp, eaLiftEither, EAAppEnv (eaAppEnvBlockfrostIpfsProjectId), eaThrow)
import Servant.Multipart (MultipartForm, MultipartData (MultipartData), Tmp, FileData (FileData))
import Servant.Multipart.Client ()
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (ErrorCall(ErrorCall))

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
    env = mkClientEnv
      manager (BaseUrl Https "ipfs.blockfrost.io" 443 "api/v0/ipfs")
  runClientM client env

-- | List pinned objects
ipfsPinnedObjects :: EAApp [IpfsListResponse]
ipfsPinnedObjects = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show =<<
    (liftIO . runClient' $ ipfsList (Just token))

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
      eaLiftEither show =<<
        ( liftIO . runClient'
            $ ipfsAdd (Just token) ("----EalabsBackendBoundary", multipart)
        )
    else
      eaThrow . ErrorCall $ "File not found: " ++ fp

-- | Pin an object
ipfsPinObject :: Text -> EAApp IpfsPin
ipfsPinObject path = do
  token <- asks eaAppEnvBlockfrostIpfsProjectId
  eaLiftEither show =<<
    (liftIO . runClient' $ ipfsPin (Just token) path)