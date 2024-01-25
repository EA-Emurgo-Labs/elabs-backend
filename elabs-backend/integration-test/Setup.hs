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
import System.Directory

data EACtx = EACtx
  { ctx :: Ctx
  , env :: EAAppEnv
  }

withEASetup ::
  FilePath ->
  FilePath ->
  IO Setup ->
  (String -> IO ()) ->
  (EACtx -> IO ()) ->
  IO ()
withEASetup scriptsFilePath sqliteFilePath ioSetup putLog kont =
  withSetup ioSetup putLog $ \ctx -> do
    metrics <- Metrics.initialize
    policyTypedScript <- readTypedScript scriptsFilePath
    rootKey <- createRootKey

    -- Delete test wallet db
    fileExists <- doesFileExist sqliteFilePath
    when fileExists $ removeFile sqliteFilePath
    -- Create Sqlite pool and run migrations
    pool <-
      runStderrLoggingT
        ( createSqlitePool
            (T.pack sqliteFilePath)
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
    kont $ EACtx ctx env
