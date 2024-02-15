module EA.Api (
  appApi,
  apiSwagger,
  apiServer,
) where

import Control.Exception (ErrorCall (ErrorCall))
import Data.Swagger (Swagger)
import EA (EAApp, eaThrow)
import EA.Api.Mint (
  MintApi (..),
  handleMintApi,
 )
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Types (AuthorizationHeader (unAuthorizationHeader))
import EA.Api.Wallet (WalletApi, handleWalletApi)
import EA.Auth (validToken)
import Servant (
  GenericMode ((:-)),
  HasServer (ServerT),
  Header,
  NamedRoutes,
  ToServantApi,
  hoistServer,
  (:>),
 )
import Servant.Swagger (HasSwagger, toSwagger)

--------------------------------------------------------------------------------

type Api =
  "api"
    :> "v0"
    :> Header "Authorization" AuthorizationHeader
    :> NamedRoutes ChangeblockApi

data ChangeblockApi mode = ChangeblockApi
  { txApi :: mode :- TxApi
  , mintApi :: mode :- NamedRoutes MintApi
  , walletApi :: mode :- WalletApi
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes ChangeblockApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi ChangeblockApi))

-- TODO:
-- type ChangeblockApi =
--   "api" :> "v0" :> CarbonApi :<|> OrderApi

apiSwagger :: Swagger
apiSwagger = toSwagger appApi

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
    , mintApi = handleMintApi
    , walletApi = handleWalletApi
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
      -- TODO: err401
      Nothing -> eaThrow . ErrorCall $ "No authentication header found."
      Just token -> do
        -- TODO: this data should be stored in the reader to prevent constant
        -- database lookups
        valid <- validToken . unAuthorizationHeader $ token
        -- TODO: err401
        unless valid (eaThrow . ErrorCall $ "Invalid token.")
        action
