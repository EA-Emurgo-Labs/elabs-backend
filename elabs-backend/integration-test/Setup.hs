module Setup (
  EACtx (..),
  withEASetup
) where

import GeniusYield.Test.Privnet.Ctx (Ctx (..), ctxProviders)
import GeniusYield.Test.Privnet.Setup (Setup, withSetup)
import EA (EAAppEnv (..))
import qualified Control.Monad.Metrics as Metrics
import Internal.Wallet (RootKey, genRootKeyFromMnemonic)
import Cardano.Mnemonic (MkSomeMnemonic(mkSomeMnemonic))
import GeniusYield.Types (GYNetworkId(GYPrivnet))
import Ply (readTypedScript)
import EA.Script (Scripts(Scripts))
import System.Directory
import qualified Data.Text as T
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (createSqlitePool)

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
          , eaAppEnvRootKey = createRootKey
          }
    kont $ EACtx ctx env
