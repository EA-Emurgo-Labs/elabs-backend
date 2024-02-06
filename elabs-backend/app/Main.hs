module Main (main) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Control.Monad.Logger (
  LoggingT (runLoggingT),
  fromLogStr,
 )
import Control.Monad.Metrics qualified as Metrics
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text qualified as T
import Database.Persist.Sqlite (
  createSqlitePool,
  runSqlPool,
 )
import EA (EAAppEnv (..))
import EA.Api (apiServer, apiSwagger, appApi)
import EA.Internal (fromLogLevel)
import EA.Script (Scripts (..))
import GeniusYield.GYConfig (
  GYCoreConfig (cfgNetworkId),
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Types (gyLog, gyLogInfo)
import Internal.Wallet (genRootKeyFromMnemonic, readRootKey, writeRootKey)
import Internal.Wallet.DB.Sqlite (runAutoMigration)
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
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
  strOption,
  subparser,
  value,
 )
import Ply (readTypedScript)
import Relude.Unsafe qualified as Unsafe
import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )
import System.Environment (getEnv)

--------------------------------------------------------------------------------

data Options = Options
  { optionsCoreConfigFile :: !String
  , optionsScriptsFile :: !String
  , optionsRootKeyFile :: !String
  , optionsCommand :: !Commands
  }

data Commands
  = RunServer ServerOptions
  | ExportSwagger SwaggerOptions
  | GenerateRootKey RootKeyOptions

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

data RootKeyOptions = RootKeyOptions
  { rootKeyOptionsMnemonic :: !String
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
      ( long "scripts"
          <> help "Scripts file"
          <> showDefault
          <> value "scripts.json"
      )
    <*> option
      auto
      ( long "root-key"
          <> help "Root key file"
          <> showDefault
          <> value "root.key"
      )
    <*> subparser
      ( command "run" (info (RunServer <$> serverOptions) (progDesc "Run backend server"))
          <> command "swagger" (info (ExportSwagger <$> swaggerOptions) (progDesc "Export swagger api"))
          <> command "genrootkey" (info (GenerateRootKey <$> rootKeyOptions) (progDesc "Root key generation"))
      )

rootKeyOptions :: Parser RootKeyOptions
rootKeyOptions =
  RootKeyOptions
    <$> strOption
      ( long "mnemonic"
          <> help "Mnemonic (15 words)"
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
        RunServer (ServerOptions {..}) ->
          do
            -- read .env in the environment
            loadFile defaultConfig

            -- TODO: This is one particular script
            --       -> Make FromJSON instance of Scripts
            policyTypedScript <- readTypedScript optionsScriptsFile
            carbonTypedScript <- readTypedScript "contracts/carbon.json"
            marketplaceTypedScript <- readTypedScript "contracts/marketplace.json"
            oracleTypedScript <- readTypedScript "contracts/oracle.json"
            mintingNftTypedScript <- readTypedScript "contracts/nft.json"
            let scripts =
                  Scripts
                    { scriptsOneShotPolicy = policyTypedScript
                    , scriptCarbonPolicy = carbonTypedScript
                    , scriptMintingNftPolicy = mintingNftTypedScript
                    , scriptMarketplaceValidator = marketplaceTypedScript
                    , scriptOracleMintingPolicy = oracleTypedScript
                    }

            metrics <- Metrics.initialize

            -- Create Sqlite pool and run migrations
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
            -- migrate tables
            void $ runSqlPool runAutoMigration pool

            rootKey <- Unsafe.fromJust <$> readRootKey optionsRootKeyFile

            bfIpfsToken <- getEnv "BLOCKFROST_IPFS"

            let
              env =
                EAAppEnv
                  { eaAppEnvGYProviders = providers
                  , eaAppEnvGYNetworkId = cfgNetworkId conf
                  , eaAppEnvMetrics = metrics
                  , eaAppEnvScripts = scripts
                  , eaAppEnvSqlPool = pool
                  , eaAppEnvRootKey = rootKey
                  , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
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
        GenerateRootKey (RootKeyOptions {..}) -> do
          mw <-
            either
              (const (error "Invalid mnemonic"))
              return
              (mkSomeMnemonic @'[15] (words $ T.pack rootKeyOptionsMnemonic))
          writeRootKey optionsRootKeyFile $ genRootKeyFromMnemonic mw

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
