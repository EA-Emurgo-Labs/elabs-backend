{-# OPTIONS_GHC -Wno-deprecations #-}
module EA.Api (
  appApi,
  apiSwagger,
  apiServer,
) where

import Data.Swagger (Swagger)
import EA (EAApp)
import EA.Api.Mint (
  MintApi (..),
  handleMintApi,
 )
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Wallet (WalletApi, handleWalletApi)
import Servant (
  GenericMode ((:-)),
  NamedRoutes,
  (:>), HasServer (ServerT), ToServantApi,
 )
import Servant.Swagger (toSwagger, HasSwagger)

--------------------------------------------------------------------------------

type Api =
  "api"
    :> "v0"
    -- :> Header "Authorization" AuthorizationHeader
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
apiSwagger =  toSwagger appApi

appApi :: Proxy Api
appApi = Proxy

apiServer :: ServerT Api EAApp
apiServer =
  ChangeblockApi
    { txApi = handleTxApi
    , mintApi = handleMintApi
    , walletApi = handleWalletApi
    }
