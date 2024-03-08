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
import Data.List qualified as List
import Data.Text qualified as T
import Database.Persist.Sqlite (
  createSqlitePool,
  runSqlPool,
 )
import EA (EAAppEnv (..), eaLiftMaybe, eaSubmitTx, runEAApp)
import EA.Api (apiSwagger)
import EA.Internal (fromLogLevel)
import EA.Routes (appRoutes, routes)
import EA.Script (Scripts (..), nftMintingPolicy, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Script.Oracle (oracleNftAsset, utxoToOracleInfo)
import EA.Tx.Changeblock.Marketplace (deployScript)
import EA.Tx.Changeblock.Oracle (createOracle)
import EA.Wallet (eaGetCollateralFromInternalWallet, eaGetInternalAddresses, eaSelectOref)
import GeniusYield.GYConfig (
  GYCoreConfig (cfgNetworkId),
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Imports (printf)
import GeniusYield.TxBuilder (addressToPubKeyHashIO, runGYTxMonadNode)
import GeniusYield.Types
import Internal.Wallet (
  genRootKeyFromMnemonic,
  readRootKey,
  writeRootKey,
 )
import Internal.Wallet qualified as Wallet
import Internal.Wallet.DB.Sql (
  addToken,
  createAccount,
  getTokens,
  runAutoMigration,
 )
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
  switch,
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
  , optionsRootKeyFile :: !String
  , optionsCommand :: !Commands
  }

data AuthTokenOptions = AuthTokenOptions
  { authtokenOptionsToken :: !String
  , authtokenOptionsNotes :: !String
  , authtokenOptionsServerOptions :: !ServerOptions
  }

data InternalAddressesOptions = InternalAddressesOptions
  { internalAddressesCollateral :: !Bool
  , internalAddressesServerOptions :: !ServerOptions
  }

data Commands
  = RunServer ServerOptions
  | ExportSwagger SwaggerOptions
  | GenerateRootKey RootKeyOptions
  | PrintInternalAddresses InternalAddressesOptions
  | PrintAuthTokens ServerOptions
  | AddAuthTokens AuthTokenOptions
  | CreateOracle CreateOracleOptions
  | DeployScript DeployMarketplaceScriptOptions

data CreateOracleOptions = CreateOracleOptions
  { createOracleServerOptions :: !ServerOptions
  , createOracleOptionsRate :: !Int
  , createOracleOptionsAssetName :: !String
  }
  deriving stock (Show, Read)

data DeployMarketplaceScriptOptions = DeployMarketplaceScriptOptions
  { dplMktplaceServerOptions :: !ServerOptions
  , dplMktplaceAddress :: !Text
  }
  deriving stock (Show, Read)

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
      ( long "root-key"
          <> help "Root key file"
          <> showDefault
          <> value "root.key"
      )
    <*> subparser
      ( command "run" (info (RunServer <$> serverOptions) (progDesc "Run backend server"))
          <> command "swagger" (info (ExportSwagger <$> swaggerOptions) (progDesc "Export swagger api"))
          <> command "genrootkey" (info (GenerateRootKey <$> rootKeyOptions) (progDesc "Root key generation"))
          <> command "internaladdresses" (info (PrintInternalAddresses <$> internalAddressesOptions) (progDesc "Print internal addresses"))
          <> command "tokens" (info (PrintAuthTokens <$> serverOptions) (progDesc "Print available auth tokens"))
          <> command "addtoken" (info (AddAuthTokens <$> authTokenOptions) (progDesc "Add new token"))
          <> command "createOracle" (info (CreateOracle <$> createOracleOptions) (progDesc "Create oracle"))
          <> command "deployScript" (info (DeployScript <$> deployScriptOption) (progDesc "Deploy Marketplace Script"))
      )

createOracleOptions :: Parser CreateOracleOptions
createOracleOptions =
  CreateOracleOptions
    <$> serverOptions
    <*> option
      auto
      ( long "rate"
          <> help "Rate"
      )
    <*> strOption
      ( long "asset"
          <> help "Asset name"
          <> showDefault
          <> value "43424c"
      )

deployScriptOption :: Parser DeployMarketplaceScriptOptions
deployScriptOption =
  DeployMarketplaceScriptOptions
    <$> serverOptions
    <*> strOption
      ( long "address"
          <> help "Address"
          <> showDefault
          <> value "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"
      )

internalAddressesOptions :: Parser InternalAddressesOptions
internalAddressesOptions =
  InternalAddressesOptions
    <$> switch (long "collateral" <> help "Collateral")
    <*> serverOptions

authTokenOptions :: Parser AuthTokenOptions
authTokenOptions =
  AuthTokenOptions
    <$> strOption
      ( long "token"
          <> help "Auth token"
      )
    <*> strOption
      ( long "notes"
          <> help "Notes"
      )
    <*> serverOptions

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
app opts@(Options {..}) = do
  conf <- coreConfigIO optionsCoreConfigFile

  withCfgProviders conf "app" $
    \providers -> do
      case optionsCommand of
        RunServer srvopts@(ServerOptions {..}) ->
          do
            env <- initEAApp conf providers opts srvopts
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
        PrintInternalAddresses (InternalAddressesOptions {..}) -> do
          env <-
            initEAApp conf providers opts internalAddressesServerOptions
          addrs <-
            runEAApp env $ eaGetInternalAddresses internalAddressesCollateral
          putTextLn . show $ fst <$> addrs
        PrintAuthTokens srvOpts -> do
          env <- initEAApp conf providers opts srvOpts
          putTextLn . show $ env.eaAppEnvAuthTokens
        AddAuthTokens (AuthTokenOptions {..}) -> do
          env <- initEAApp conf providers opts authtokenOptionsServerOptions
          pool <- runEAApp env $ asks eaAppEnvSqlPool
          runSqlPool
            (addToken (T.pack authtokenOptionsToken) (T.pack authtokenOptionsNotes))
            pool
        CreateOracle (CreateOracleOptions {..}) -> do
          env <- initEAApp conf providers opts createOracleServerOptions
          internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False

          -- Get the collateral address and its signing key.
          (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

          (addr, key, oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

          -- TODO: User proper policyId for Oracle NFT
          let operatorPubkeyHash = eaAppEnvOracleOperatorPubKeyHash env
              scripts = eaAppEnvScripts env
              networkId = eaAppEnvGYNetworkId env
              orcNftPolicy = nftMintingPolicy oref scripts
              oracleNftAsset = mintingPolicyId orcNftPolicy
              orcTokenName = unsafeTokenNameFromHex $ T.pack createOracleOptionsAssetName
              orcAssetClass = GYToken oracleNftAsset orcTokenName
              orcValidator = oracleValidator orcAssetClass operatorPubkeyHash scripts
              orcAddress = addressFromValidator networkId orcValidator
              skeleton = createOracle (fromIntegral createOracleOptionsRate) oref orcAddress orcTokenName orcNftPolicy

          txBody <-
            liftIO $
              runGYTxMonadNode networkId providers [addr] addr collateral (return skeleton)

          gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]
          printf "\n Oracle created with TxId: %s \n " gyTxId
          printf "\n Operator pubkeyHash: %s \n Operator Address: %s \n" operatorPubkeyHash addr
          printf "\n Oracle NFT Asset: %s \n" orcAssetClass
          printf "\n Oracle Address: %s \n" orcAddress
          printf "\n \n export ORACLE_UTXO_REF=%s#0 \n" gyTxId
        DeployScript (DeployMarketplaceScriptOptions {..}) -> do
          printf "Deploying Marketplace Script to Address: %s" dplMktplaceAddress
          env <- initEAApp conf providers opts dplMktplaceServerOptions
          internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False
          oracleNftPolicyId <- runEAApp env $ asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybe "No Oracle NFT Policy Id"
          oracleNftTknName <- runEAApp env $ asks eaAppEnvOracleNftTokenName >>= eaLiftMaybe "No Oracle NFT Token Name"
          escrowPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceEscrowPubKeyHash
          version <- runEAApp env $ asks eaAppEnvMarketplaceVersion
          networkId <- runEAApp env $ asks eaAppEnvGYNetworkId

          -- Get the collateral address and its signing key.
          (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

          (addr, key, _oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

          let scripts = eaAppEnvScripts env
              oracleValidatorHash = validatorHash $ oracleValidator (GYToken oracleNftPolicyId oracleNftTknName) (eaAppEnvOracleOperatorPubKeyHash env) scripts
              marketplaceParams =
                MarketplaceParams
                  { mktPrmOracleValidator = oracleValidatorHash
                  , mktPrmEscrowValidator = escrowPubkeyHash
                  , mktPrmVersion = version
                  , mktPrmOracleSymbol = oracleNftPolicyId
                  , mktPrmOracleTokenName = oracleNftTknName
                  }
              skeleton = deployScript (unsafeAddressFromText dplMktplaceAddress) marketplaceParams scripts

          txBody <-
            liftIO $
              runGYTxMonadNode networkId providers [addr] addr collateral (return skeleton)

          gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

          printf "\n \n export MARKETPLACE_REF_SCRIPT_UTXO=%s#0 \n" gyTxId

initEAApp :: GYCoreConfig -> GYProviders -> Options -> ServerOptions -> IO EAAppEnv
initEAApp conf providers (Options {..}) (ServerOptions {..}) = do
  -- read .env in the environment
  loadFile defaultConfig

  carbonTokenTypedScript <- readTypedScript "contracts/carbon-token.json"
  carbonNftTypedScript <- readTypedScript "contracts/carbon-nft.json"
  marketplaceTypedScript <- readTypedScript "contracts/marketplace.json"
  oracleTypedScript <- readTypedScript "contracts/oracle.json"
  mintingNftTypedScript <- readTypedScript "contracts/nft.json"

  let scripts =
        Scripts
          { scriptCarbonNftPolicy = carbonNftTypedScript
          , scriptCarbonTokenPolicy = carbonTokenTypedScript
          , scriptMintingNftPolicy = mintingNftTypedScript
          , scriptMarketplaceValidator = marketplaceTypedScript
          , scriptOracleValidator = oracleTypedScript
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
  tokens <-
    runSqlPool
      (runAutoMigration >> createAccount >> getTokens)
      pool

  rootKey <- Unsafe.fromJust <$> readRootKey optionsRootKeyFile

  bfIpfsToken <- getEnv "BLOCKFROST_IPFS"

  -- Get Oracle Info for reference input
  oracleRefInputUtxo <-
    lookupEnv "ORACLE_UTXO_REF"
      >>= maybe (pure []) (gyQueryUtxosAtTxOutRefsWithDatums providers . List.singleton . fromString)
      >>= maybe (pure Nothing) (return . rightToMaybe . utxoToOracleInfo) . listToMaybe

  (oracleNftPolicyId, oracleNftTokenName) <-
    maybe
      (return (Nothing, Nothing))
      (return . oracleNftPolicyIdAndTokenName . oracleNftAsset)
      oracleRefInputUtxo

  -- Oracle Operator and Escrow PubkeyHash
  operatorPubkeyHash <- addressToPubKeyHashIO $ oracleOperatorAddress (cfgNetworkId conf)
  escrowPubkeyHash <- addressToPubKeyHashIO $ escrowAddress (cfgNetworkId conf)

  -- Get Marketplace Utxo for reference script
  marketplaceRefScriptUtxo <-
    lookupEnv "MARKETPLACE_REF_SCRIPT_UTXO"
      >>= maybe (return Nothing) (gyQueryUtxoAtTxOutRef providers . fromString)

  return $
    EAAppEnv
      { eaAppEnvGYProviders = providers
      , eaAppEnvGYNetworkId = cfgNetworkId conf
      , eaAppEnvMetrics = metrics
      , eaAppEnvScripts = scripts
      , eaAppEnvSqlPool = pool
      , eaAppEnvRootKey = rootKey
      , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
      , eaAppEnvAuthTokens = tokens
      , eaAppEnvOracleRefInputUtxo = oracleRefInputUtxo
      , eaAppEnvMarketplaceRefScriptUtxo = utxoRef <$> marketplaceRefScriptUtxo
      , eaAppEnvOracleOperatorPubKeyHash = operatorPubkeyHash
      , eaAppEnvOracleNftMintingPolicyId = oracleNftPolicyId
      , eaAppEnvOracleNftTokenName = oracleNftTokenName
      , eaAppEnvMarketplaceEscrowPubKeyHash = escrowPubkeyHash
      , eaAppEnvMarketplaceVersion = unsafeTokenNameFromHex "76302e302e33" -- v0.0.3
      }
  where
    oracleNftPolicyIdAndTokenName :: Maybe GYAssetClass -> (Maybe GYMintingPolicyId, Maybe GYTokenName)
    oracleNftPolicyIdAndTokenName (Just (GYToken policyId tokename)) = (Just policyId, Just tokename)
    oracleNftPolicyIdAndTokenName _ = (Nothing, Nothing)

    -- TODO: Use valid Escrow & oracle Operator  address
    escrowAddress :: GYNetworkId -> GYAddress
    escrowAddress _ = unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"

    oracleOperatorAddress :: GYNetworkId -> GYAddress
    oracleOperatorAddress _ = unsafeAddressFromText "addr_test1qruxukp4fdncrcnxds6ze2afcufs8w4a6m02a0u7yucppwfx23xw3uj9gkatk450ac7hec80ujfyvk3c97f7n8eljjrq74zl3e"

server :: EAAppEnv -> Application
server env =
  cors
    ( const $
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = [HttpTypes.hContentType] -- FIXME: better CORS policy
            }
    )
    $ serve appRoutes
    $ hoistServer appRoutes (Handler . ExceptT . try . runEAApp env) routes
