module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text qualified as T

import Control.Exception (try)
import Control.Monad.Logger (
  LoggingT (runLoggingT),
  fromLogStr,
 )
import Control.Monad.Metrics qualified as Metrics

import Options.Applicative (
  Parser,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  option,
  progDesc,
  short,
  showDefault,
  subparser,
  value,
 )

import GeniusYield.GYConfig (
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Types (gyLog, gyLogInfo)

import Ply (readTypedScript)

import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )

import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )

import Database.Persist.Sqlite (createSqlitePool)

import EA (EAAppEnv (..))
import EA.Api (apiServer, apiSwagger, appApi)
import EA.Internal (fromLogLevel)
import EA.Script (Scripts (Scripts))

data Options = Options
  { optionsCoreConfigFile :: !String
  , optionsScriptsFile :: !String
  , optionsCommand :: !Commands
  }

data Commands
  = RunServer ServerOptions
  | ExportSwagger SwaggerOptions

data ServerOptions = ServerOptions
  { serverOptionsPort :: !Int
  , serverOptionsSqliteFile :: !String
  , serverOptionsSqlitePoolSize :: !Int
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
      ( long "scripts"
          <> help "Scripts file"
          <> showDefault
          <> value "scripts.json"
      )
    <*> subparser
      ( command "run" (info (RunServer <$> serverOptions) (progDesc "Run backend server"))
          <> command "swagger" (info (ExportSwagger <$> swaggerOptions) (progDesc "Export swagger api"))
      )

serverOptions :: Parser ServerOptions
serverOptions =
  ServerOptions
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port"
          <> showDefault
          <> value 8081
      )
    <*> option
      auto
      ( long "sqlite"
          <> help "Sqlite file"
          <> showDefault
          <> value "wallet.db"
      )
    <*> option
      auto
      ( long "sqlite-pool-size"
          <> help "Sqlite pool size"
          <> showDefault
          <> value 10
      )

swaggerOptions :: Parser SwaggerOptions
swaggerOptions =
  SwaggerOptions
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
app (Options {..}) = do
  conf <- coreConfigIO optionsCoreConfigFile

  withCfgProviders conf "app" $
    \providers -> do
      case optionsCommand of
        RunServer (ServerOptions {..}) -> do
          -- TODO: This is one particular script
          --       -> Make FromJSON instance of Scripts
          policyTypedScript <- readTypedScript optionsScriptsFile
          metrics <- Metrics.initialize

          pool <-
            runLoggingT
              ( createSqlitePool
                  (T.pack serverOptionsSqliteFile)
                  serverOptionsSqlitePoolSize
              )
              $ \_ _ lvl msg ->
                gyLog
                  providers
                  "db"
                  (fromLogLevel lvl)
                  (decodeUtf8 $ fromLogStr msg)

          let
            env =
              EAAppEnv
                { eaAppEnvGYProviders = providers
                , eaAppEnvGYCoreConfig = conf
                , eaAppEnvMetrics = metrics
                , eaAppEnvScripts = Scripts policyTypedScript
                , eaAppEnvSqlPool = pool
                }
          gyLogInfo providers "app" $
            "Starting server at "
              <> "http://localhost:"
              <> show serverOptionsPort
          run serverOptionsPort $ server env
        ExportSwagger (SwaggerOptions {..}) -> do
          let file = swaggerOptionsFile
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
