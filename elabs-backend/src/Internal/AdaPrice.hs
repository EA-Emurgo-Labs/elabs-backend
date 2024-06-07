module Internal.AdaPrice (
  AdaPriceResponse (..),
  getAdaPrice,
) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (
  Get,
  JSON,
  QueryParam,
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

import Data.Aeson qualified as Aeson

data AdaPriceResponse = AdaPriceResponse
  { symbol :: String
  , price :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

type AdaPriceApi =
  "ticker"
    :> "price"
    :> QueryParam "symbol" String
    :> Get '[JSON] AdaPriceResponse

api :: Proxy AdaPriceApi
api = Proxy

runAdaPriceClient' :: ClientM a -> IO (Either ClientError a)
runAdaPriceClient' client = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.binance.com" 443 "api/v3"
  runClientM client (mkClientEnv manager baseUrl)

getAdaPrice :: IO (Maybe String)
getAdaPrice = do
  resp <- runAdaPriceClient' $ client api (Just "ADAUSDT")
  case resp of
    Left _ -> return Nothing
    Right (AdaPriceResponse {..}) -> return $ Just price