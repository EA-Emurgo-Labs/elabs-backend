module EA.Routes (appRoutes, routes) where

import EA.Api (Api, apiServer, apiSwagger)
import Servant (
  (:-),
  (:>),
  HasServer (ServerT),
  NamedRoutes,
  Get,
  JSON
 )
import EA (EAApp)
import Data.Swagger (Swagger)

--------------------------------------------------------------------------------

data Routes mode = Routes
  { api :: mode :- "api" :> Api
  , swagger :: mode :- "swagger.json" :> Get '[JSON] Swagger
  }
  deriving stock (Generic)

appRoutes :: Proxy (NamedRoutes Routes)
appRoutes = Proxy

routes :: ServerT (NamedRoutes Routes) EAApp
routes =
  Routes
    { api = apiServer
    , swagger = pure apiSwagger
    }