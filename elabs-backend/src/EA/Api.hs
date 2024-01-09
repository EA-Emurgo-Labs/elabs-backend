module EA.Api (
  appApi,
  apiServer,
) where

import Servant

import EA (EAAppEnv, runEAApp)
import EA.Api.Tx (TxApi, handleTxApi)

--------------------------------------------------------------------------------

type Api =
  TxApi
  -- :<|> TODO:

appApi :: Proxy Api
appApi = Proxy

apiServer :: EAAppEnv -> ServerT Api IO
apiServer env =
  runEAApp env . handleTxApi
