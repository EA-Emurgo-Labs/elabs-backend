module EA.Internal (
  validatorFromPly,
  mintingPolicyFromPly,
  fromLoc,
  fromLogLevel,
) where

import Control.Monad.Logger (Loc (..), LogLevel (..))

import GeniusYield.Types (
  GYLogSeverity (..),
  GYMintingPolicy,
  GYValidator,
  PlutusVersion (PlutusV2),
  mintingPolicyFromSerialisedScript,
  validatorFromSerialisedScript,
 )
import GeniusYield.Types.Logging (GYLogNamespace)

import PlutusLedgerApi.V2 (serialiseUPLC)

import Ply (TypedScript (TypedScript))

--------------------------------------------------------------------------------

validatorFromPly ::
  forall r.
  TypedScript r '[] ->
  GYValidator 'PlutusV2
validatorFromPly (TypedScript _ s) =
  validatorFromSerialisedScript @'PlutusV2 $ serialiseUPLC s

mintingPolicyFromPly ::
  forall r.
  TypedScript r '[] ->
  GYMintingPolicy 'PlutusV2
mintingPolicyFromPly (TypedScript _ s) =
  mintingPolicyFromSerialisedScript @'PlutusV2 $ serialiseUPLC s

--------------------------------------------------------------------------------
--- Internal logging helpers

fromLoc :: Loc -> GYLogNamespace
fromLoc (Loc {..}) =
  fromString loc_package
    <> fromString loc_module
    <> fromString loc_filename

fromLogLevel :: LogLevel -> GYLogSeverity
fromLogLevel LevelDebug = GYDebug
fromLogLevel LevelInfo = GYInfo
fromLogLevel LevelWarn = GYWarning
fromLogLevel LevelError = GYError
fromLogLevel (LevelOther _) = GYError
