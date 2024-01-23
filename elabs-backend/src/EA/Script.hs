module EA.Script (Scripts (..), oneShotMintingPolicy) where

import EA.Internal (mintingPolicyFromPly)
import GeniusYield.Types
import PlutusLedgerApi.V2 (TxOutRef)
import Ply (AsData (AsData), ScriptRole (MintingPolicyRole), TypedScript, (#))

data Scripts = Scripts
  { scriptsOneShotPolicy :: !(TypedScript 'MintingPolicyRole '[AsData TxOutRef])
  }

oneShotMintingPolicy :: GYTxOutRef -> Scripts -> GYMintingPolicy 'PlutusV2
oneShotMintingPolicy oref scripts =
  mintingPolicyFromPly $
    scriptsOneShotPolicy scripts
      # (AsData . txOutRefToPlutus $ oref)
