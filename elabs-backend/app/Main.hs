module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8

import Control.Exception (try)
import Control.Monad.Metrics qualified as Metrics

import Options.Applicative

import GeniusYield.GYConfig (
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Types (gyLogInfo)

import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

import Servant

import EA (EAAppEnv (..))
import EA.Api (apiServer, apiSwagger, appApi)
import EA.Script (Scripts (Scripts))
import Ply (readTypedScript)

data Options = Options
  { optionsCoreConfigFile :: !String
  , optionsLogNameSpace :: !String
  , optionsScriptsFile :: !String
  , optionsCommand :: !Commands
  }

data Commands
  = RunServer ServerOptions
  | ExportSwagger SwaggerOptions

data ServerOptions = ServerOptions
  { serverOptionsPort :: !Int
  }
  deriving stock (Show, Read)

data SwaggerOptions = SwaggerOptions
  { swaggerOptionsFile :: !String
  }
  deriving stock (Show, Read)

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "core-config"
          <> help "Core config file path"
          <> showDefault
          <> value "config.json"
      )
    <*> option
      auto
      ( long "log-namespace"
          <> help "Log namespace"
          <> showDefault
          <> value "elabs-backend"
      )
    <*> option
      auto
      ( long "scripts"
          <> help "Scripts file"
          <> showDefault
          <> value "scripts.json"
      )
    <*> subparser
      ( command "run" (info serverOptions (progDesc "Run backend server"))
          <> command "swagger" (info swaggerOptions (progDesc "Export swagger api"))
      )

serverOptions :: Parser Commands
serverOptions =
  RunServer
    . ServerOptions
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port"
          <> showDefault
          <> value 8081
      )

swaggerOptions :: Parser Commands
swaggerOptions =
  ExportSwagger
    . SwaggerOptions
    <$> option
      auto
      ( long "outfile"
          <> short 'f'
          <> help "Swagger file output"
          <> showDefault
          <> value "swagger-api.json"
      )

main :: IO ()
main = app =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "EMURGUO Africa Backend"
            <> header "elabs-backend - Backend Server"
        )

app :: Options -> IO ()
app opts = do
  metrics <- Metrics.initialize
  conf <- coreConfigIO $ optionsCoreConfigFile opts

  withCfgProviders conf (fromString $ optionsLogNameSpace opts) $
    \providers -> do
      case optionsCommand opts of
        RunServer opt -> do
          -- TODO: This is one particular script
          --       -> Make FromJSON instance of Scripts
          policyTypedScript <- readTypedScript (optionsScriptsFile opts)

          let
            port = serverOptionsPort opt
            scripts = Scripts policyTypedScript
            env =
              EAAppEnv
                { eaAppEnvGYProviders = providers
                , eaAppEnvGYCoreConfig = conf
                , eaAppEnvMetrics = metrics
                , eaAppEnvScripts = scripts
                }
          gyLogInfo providers "app" $
            "Starting server at "
              <> "http://localhost:"
              <> show port
          run port $ server env
        ExportSwagger opt -> do
          let file = swaggerOptionsFile opt
          gyLogInfo providers "app" $ "Writting swagger file to " <> file
          BL8.writeFile file (encodePretty apiSwagger)

server :: EAAppEnv -> Application
server env =
  cors
    ( const $
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = [HttpTypes.hContentType] -- FIXME: better CORS policy
            }
    )
    $ serve appApi
    $ hoistServer appApi (Handler . ExceptT . try)
    $ apiServer env
