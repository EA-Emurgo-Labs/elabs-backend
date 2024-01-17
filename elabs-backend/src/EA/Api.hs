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
  handleOneShotMintByUserId,
  handleOneShotMintByWallet,
 )
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Wallet (WalletApi, handleWalletApi)

--------------------------------------------------------------------------------

type Api =
  TxApi :<|> MintApi :<|> WalletApi

apiSwagger :: Swagger
apiSwagger = toSwagger appApi

appApi :: Proxy Api
appApi = Proxy

apiServer :: EAAppEnv -> ServerT Api IO
apiServer env =
  runEAApp env . handleTxApi
    :<|> ( runEAApp env . handleOneShotMintByWallet
            :<|> runEAApp env . handleOneShotMintByUserId
         )
    :<|> runEAApp env . handleWalletApi
