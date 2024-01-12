module EA.Internal (
  validatorFromPly,
  mintingPolicyFromPly,
) where

import GeniusYield.Types (
  GYMintingPolicy,
  GYValidator,
  PlutusVersion (PlutusV2),
  mintingPolicyFromSerialisedScript,
  validatorFromSerialisedScript,
 )
import PlutusLedgerApi.V2 (serialiseUPLC)
import Ply (TypedScript (TypedScript))

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
