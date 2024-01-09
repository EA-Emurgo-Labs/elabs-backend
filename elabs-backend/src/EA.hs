module EA (
  EAApp (..),
  EAAppEnv (..),
  runEAApp,
) where

import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))

import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (
  GYProviders,
 )

--------------------------------------------------------------------------------

newtype EAApp a = EAApp
  { unEAApp :: ReaderT EAAppEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader EAAppEnv)

instance MonadMetrics EAApp where
  getMetrics = asks eaAppEnvMetrics

data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYCoreConfig :: !GYCoreConfig
  , eaAppEnvMetrics :: !Metrics
  -- , eaAppEnvScripts :: !Scripts
  }

runEAApp :: EAAppEnv -> EAApp a -> IO a
runEAApp env = flip runReaderT env . unEAApp
