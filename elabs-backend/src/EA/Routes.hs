module EA.Routes (appRoutes, routes) where

import Data.Swagger (Swagger)
import EA (EAApp)
import EA.Api (Api, apiServer, apiSwagger, apiSwaggerUI)
import Servant (
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Raw,
  (:-),
  (:>),
 )

--------------------------------------------------------------------------------

data Routes mode = Routes
  { api :: mode :- "api" :> Api
  , swagger :: mode :- "swagger.json" :> Get '[JSON] Swagger
  , swaggerUi :: mode :- "swagger-ui" :> Raw
  }
  deriving stock (Generic)

appRoutes :: Proxy (NamedRoutes Routes)
appRoutes = Proxy

routes :: ServerT (NamedRoutes Routes) EAApp
routes =
  Routes
    { api = apiServer
    , swagger = pure apiSwagger
    , swaggerUi = apiSwaggerUI
    }
