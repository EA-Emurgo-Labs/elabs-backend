module Setup (
  EACtx (..),
  withEASetup,
  server,
  cleanupSetup,
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Metrics qualified as Metrics
import Data.List ((!!))
import Data.Text qualified as T
import Database.Persist.Sqlite (createSqlitePool, rawExecute, runSqlPool)
import EA (EAAppEnv (..), eaLiftMaybe, runEAApp)
import EA.Api (apiServer, appApi)
import EA.Script (Scripts (Scripts))
import EA.Test.Helpers (createRootKey)
import EA.Wallet (eaGetInternalAddresses)
import GeniusYield.Test.Privnet.Ctx (
  Ctx (..),
  ctxProviders,
  ctxRunI,
  submitTx,
 )
import GeniusYield.Test.Privnet.Setup (Setup, withSetup)
import GeniusYield.TxBuilder (mustHaveOutput)
import GeniusYield.Types (
  GYNetworkId (GYPrivnet),
  GYTxOut (GYTxOut),
  valueFromLovelace,
 )
import Internal.Wallet.DB.Sqlite (
  createAccount,
  runAutoMigration,
 )
import Ply (readTypedScript)
import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnv)
import System.FilePath.Glob (glob)
import System.Random (randomRIO)

--------------------------------------------------------------------------------

data EACtx = EACtx
  { eaCtxCtx :: Ctx
  , eaCtxEnv :: EAAppEnv
  }

withEASetup ::
  IO Setup ->
  (String -> IO ()) ->
  (EACtx -> IO ()) ->
  IO ()
withEASetup ioSetup putLog kont =
  withSetup ioSetup putLog $ \ctx -> do
    -- read .env file
    loadFile defaultConfig

    id <- randomString 10
    let
      -- TODO: load this dynamically, also check cleanupSetup function
      optionsScriptsFile = "scripts.debug.json"
      optionsSqliteFile = "wallet.test." <> id <> ".db"

    metrics <- Metrics.initialize
    policyTypedScript <- readTypedScript optionsScriptsFile
    rootKey <- createRootKey

    -- Create Sqlite pool and run migrations
    pool <-
      runStderrLoggingT
        ( createSqlitePool
            (T.pack optionsSqliteFile)
            20
        )

    bfIpfsToken <- getEnv "BLOCKFROST_IPFS"

    let
      env =
        EAAppEnv
          { eaAppEnvGYProviders = ctxProviders ctx
          , eaAppEnvGYNetworkId = GYPrivnet
          , eaAppEnvMetrics = metrics
          , eaAppEnvScripts = Scripts policyTypedScript
          , eaAppEnvSqlPool = pool
          , eaAppEnvRootKey = rootKey
          , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
          }

    -- DB migrations
    void $
      runSqlPool
        ( rawExecute "PRAGMA busy_timeout=100000" []
            >> runAutoMigration
            >> createAccount
        )
        pool

    -- Adding funds to the internal collateral address
    txId <- runEAApp env $ do
      addrs <- eaGetInternalAddresses
      (addr, _) <-
        eaLiftMaybe "No internal address found" $
          viaNonEmpty head addrs
      let
        user = ctxUser2 ctx
        tx =
          mustHaveOutput
            (GYTxOut addr (valueFromLovelace 5_000_000) Nothing Nothing)
      txBody <- liftIO $ ctxRunI ctx user $ return tx
      liftIO $ submitTx ctx user txBody

    putLog $ "Send 5 Ada to the collateral address: " <> show txId

    kont $ EACtx ctx env

server :: EAAppEnv -> Application
server env =
  serve appApi $
    hoistServer appApi (Handler . ExceptT . try) $
      apiServer env

randomString :: Int -> IO String
randomString len = replicateM len randomChar
  where
    randomChar :: IO Char
    randomChar = do
      let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
      randomRIO (0, length chars - 1) <&> (chars !!)

cleanupSetup :: Setup -> IO ()
cleanupSetup _ = do
  files <- glob "wallet.test.*.db*" -- FIXME: check optionsSqliteFile
  mapM_ (\file -> doesFileExist file >>= flip when (removeFile file)) files
