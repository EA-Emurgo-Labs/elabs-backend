module EA (
  EAApp (..),
  EAAppEnv (..),
  runEAApp,
  eaLog,
  eaLogDebug,
  eaLogInfo,
  eaLogWarning,
  eaLogError,
  eaThrow,
  eaCatch,
  eaHandle,
  oneShotMintingPolicy,
) where

import Control.Exception (catch, throwIO)
import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))

import UnliftIO (MonadUnliftIO (withRunInIO))

import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (
  GYLogNamespace,
  GYLogSeverity,
  GYMintingPolicy,
  GYProviders,
  GYTxOutRef,
  PlutusVersion (PlutusV2),
  gyLog,
  gyLogDebug,
  gyLogError,
  gyLogInfo,
  gyLogWarning,
  txOutRefToPlutus,
 )

import Ply (
  AsData (AsData),
  PlyArg,
  ScriptRole (MintingPolicyRole),
  TypedScript,
  (#),
 )
import Ply.Core.Class (PlyArg (..))

import EA.Helpers (mintingPolicyFromPly)
import EA.Script (Scripts (..))

--------------------------------------------------------------------------------

newtype EAApp a = EAApp
  { unEAApp :: ReaderT EAAppEnv IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader EAAppEnv
    , MonadUnliftIO
    )

instance MonadMetrics EAApp where
  getMetrics = asks eaAppEnvMetrics

data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYCoreConfig :: !GYCoreConfig
  , eaAppEnvMetrics :: !Metrics
  , eaAppEnvScripts :: !Scripts
  }

runEAApp :: EAAppEnv -> EAApp a -> IO a
runEAApp env = flip runReaderT env . unEAApp

--------------------------------------------------------------------------------
-- Logging

eaLog :: (HasCallStack) => GYLogNamespace -> GYLogSeverity -> String -> EAApp ()
eaLog name sev msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLog providers name sev msg

eaLogDebug :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogDebug name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogDebug providers name msg

eaLogInfo :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogInfo name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogInfo providers name msg

eaLogWarning :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogWarning name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogWarning providers name msg

eaLogError :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogError name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogError providers name msg

--------------------------------------------------------------------------------
-- Exception

eaThrow :: (HasCallStack, Exception e) => e -> EAApp a
eaThrow = liftIO . throwIO

eaCatch :: (HasCallStack, Exception e) => EAApp a -> (e -> EAApp a) -> EAApp a
eaCatch action handle = withRunInIO $ \run -> run action `catch` (run . handle)

eaHandle :: (HasCallStack, Exception e) => (e -> EAApp a) -> EAApp a -> EAApp a
eaHandle = flip eaCatch

--------------------------------------------------------------------------------
-- Reader helpers

oneShotMintingPolicy :: GYTxOutRef -> EAAppEnv -> GYMintingPolicy 'PlutusV2
oneShotMintingPolicy oref =
  applyToMintingPolicy (AsData . txOutRefToPlutus $ oref) scriptsOneShotPolicy

applyToScript ::
  forall r a.
  (PlyArg a, ToDataConstraint a) =>
  AsData a ->
  (Scripts -> TypedScript r '[AsData a]) ->
  EAAppEnv ->
  TypedScript r '[]
applyToScript a f =
  (# a) . f . eaAppEnvScripts

applyToMintingPolicy ::
  forall a.
  (PlyArg a, ToDataConstraint a) =>
  AsData a ->
  (Scripts -> TypedScript 'MintingPolicyRole '[AsData a]) ->
  EAAppEnv ->
  GYMintingPolicy 'PlutusV2
applyToMintingPolicy a f =
  mintingPolicyFromPly . applyToScript a f
