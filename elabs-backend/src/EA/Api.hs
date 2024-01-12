module EA.Api (
  appApi,
  apiServer,
  apiSwagger,
) where

import Data.Swagger (Swagger)

import Servant (HasServer (ServerT), type (:<|>) (..))
import Servant.Swagger (toSwagger)

import EA (EAAppEnv, runEAApp)
import EA.Api.Mint (
  MintApi,
  handleOneShotMintByWallet,
  handleOneShotMintByWalletId,
 )
import EA.Api.Tx (TxApi, handleTxApi)

--------------------------------------------------------------------------------

type Api =
  TxApi :<|> MintApi

apiSwagger :: Swagger
apiSwagger = toSwagger appApi

appApi :: Proxy Api
appApi = Proxy

apiServer :: EAAppEnv -> ServerT Api IO
apiServer env =
  runEAApp env . handleTxApi
    :<|> runEAApp env . handleOneShotMintByWallet
    :<|> runEAApp env . handleOneShotMintByWalletId
