module EA (
  EAApp (..),
  EAAppEnv (..),
  runEAApp,
) where

import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (
  GYProviders,
 )

import EA.Script (Scripts (..))

--------------------------------------------------------------------------------

newtype EAApp a = EAApp
  { unEAApp :: ReaderT EAAppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader EAAppEnv)

data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYCoreConfig :: !GYCoreConfig
  , eaAppEnvScripts :: !Scripts
  }

runEAApp :: EAAppEnv -> EAApp a -> IO a
runEAApp env = flip runReaderT env . unEAApp
