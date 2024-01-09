module Main (main) where

import Control.Exception (try)
import Control.Monad.Metrics qualified as Metrics

import Options.Applicative

import GeniusYield.GYConfig (coreConfigIO, withCfgProviders)

import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

import Servant

import EA (EAAppEnv (..))
import EA.Api (apiServer, appApi)

data Options = Options
  { optionsCoreConfigFile :: !String
  , optionsLogNameSpace :: !String
  , optionsPort :: !Int
  }

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
      ( long "port"
          <> help "Port"
          <> showDefault
          <> value 8081
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
      let
        port = optionsPort opts
        env =
          EAAppEnv
            { eaAppEnvGYProviders = providers
            , eaAppEnvGYCoreConfig = conf
            , eaAppEnvMetrics = metrics
            -- , eaAppEnvScripts = undefined --- TODO :: !Scripts
            }
      putStrLn $ "Starting server at \n " <> "http://localhost:" <> show port
      run port $ server env

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
