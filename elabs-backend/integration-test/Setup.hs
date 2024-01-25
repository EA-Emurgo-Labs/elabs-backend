module Setup (
  EACtx (..),
  withEASetup,
) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Metrics qualified as Metrics
import Data.Text qualified as T
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import EA (EAAppEnv (..), eaLiftMaybe, runEAApp)
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
import Internal.Wallet.DB.Sqlite
import Ply (readTypedScript)
import System.Directory (doesFileExist, removeFile)

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
    let
      -- TODO: load this dynamically
      optionsScriptsFile = "scripts.debug.json"
      optionsSqliteFile = "wallet.test.db"

    metrics <- Metrics.initialize
    policyTypedScript <- readTypedScript optionsScriptsFile
    rootKey <- createRootKey

    -- Delete test wallet db
    fileExists <- doesFileExist optionsSqliteFile
    when fileExists $ removeFile optionsSqliteFile
    -- Create Sqlite pool and run migrations
    pool <-
      runStderrLoggingT
        ( createSqlitePool
            (T.pack optionsSqliteFile)
            1
        )

    let
      env =
        EAAppEnv
          { eaAppEnvGYProviders = ctxProviders ctx
          , eaAppEnvGYNetworkId = GYPrivnet
          , eaAppEnvMetrics = metrics
          , eaAppEnvScripts = Scripts policyTypedScript
          , eaAppEnvSqlPool = pool
          , eaAppEnvRootKey = rootKey
          }

    -- DB migrations
    void $ runSqlPool (runAutoMigration >> createAccount) pool

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
