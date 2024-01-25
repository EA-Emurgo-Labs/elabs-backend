module Setup (
  EACtx (..),
  withEASetup,
) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Metrics qualified as Metrics
import Data.Text qualified as T
import Database.Persist.Sqlite (createSqlitePool)
import EA (EAAppEnv (..))
import EA.Script (Scripts (Scripts))
import EA.Test.Helpers (createRootKey)
import GeniusYield.Test.Privnet.Ctx (Ctx (..), ctxProviders)
import GeniusYield.Test.Privnet.Setup (Setup, withSetup)
import GeniusYield.Types (GYNetworkId (GYPrivnet))
import Ply (readTypedScript)
import System.Directory (doesFileExist, removeFile)

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
    -- TODO: send 5 Ada to collaternal
    kont $ EACtx ctx env
