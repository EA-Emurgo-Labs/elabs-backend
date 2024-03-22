{-# LANGUAGE NoOverloadedRecordDot #-}

module EA.Api (
  Api,
  appApi,
  apiSwagger,
  apiServer,
) where

import Control.Lens ((.~), (?~))
import Data.Swagger (
  HasDescription (description),
  HasInfo (info),
  HasTitle (title),
  HasVersion (version),
  Swagger,
 )
import EA (EAApp, EAAppEnv (eaAppEnvAuthTokens), eaThrow)
import EA.Api.Carbon (CarbonApi, handleCarbonApi)
import EA.Api.Order (OrderApi, handleOrderApi)
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Types (AuthorizationHeader (unAuthorizationHeader))
import EA.Api.Wallet (WalletApi, handleWalletApi)
import Servant (
  GenericMode ((:-)),
  HasServer (ServerT),
  Header,
  NamedRoutes,
  ToServantApi,
  err401,
  hoistServer,
  (:>),
 )
import Servant.Swagger (HasSwagger, toSwagger)

--------------------------------------------------------------------------------

type Api =
  "v0"
    :> Header "Authorization" AuthorizationHeader
    :> NamedRoutes ChangeblockApi

data ChangeblockApi mode = ChangeblockApi
  { txApi :: mode :- NamedRoutes TxApi
  , walletApi :: mode :- WalletApi
  , carbonApi :: mode :- CarbonApi
  , orderApi :: mode :- NamedRoutes OrderApi
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes ChangeblockApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi ChangeblockApi))

apiSwagger :: Swagger
apiSwagger =
  toSwagger appApi
    & info . title .~ "ChangeBlock API"
    & info . version .~ "1.0"
    & info . description ?~ "The ChangeBlock API."

appApi :: Proxy Api
appApi = Proxy

apiServer :: ServerT Api EAApp
apiServer = changeblockServer'

changeblockServer ::
  Maybe AuthorizationHeader ->
  ServerT (NamedRoutes ChangeblockApi) EAApp
changeblockServer _ =
  ChangeblockApi
    { txApi = handleTxApi
    , walletApi = handleWalletApi
    , carbonApi = handleCarbonApi
    , orderApi = handleOrderApi
    }

changeblockServer' ::
  Maybe AuthorizationHeader ->
  ServerT (NamedRoutes ChangeblockApi) EAApp
changeblockServer' maybeAuthHeader =
  hoistServer
    (Proxy @(NamedRoutes ChangeblockApi))
    run
    (changeblockServer maybeAuthHeader)
  where
    run :: EAApp a -> EAApp a
    run action = case maybeAuthHeader of
      Nothing -> eaThrow err401
      Just token -> do
        tokens <- asks eaAppEnvAuthTokens
        unless
          (unAuthorizationHeader token `elem` tokens)
          (eaThrow err401)
        action
