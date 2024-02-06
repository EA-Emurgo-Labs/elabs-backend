module EA.Script (Scripts (..), oneShotMintingPolicy) where

import EA.Internal (mintingPolicyFromPly)
import GeniusYield.Types
import PlutusLedgerApi.V1 (PubKeyHash, ScriptHash, TokenName, TxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass)

import PlutusLedgerApi.V1.Tx (TxId)
import Ply (
  AsData (AsData),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedScript,
  (#),
 )

data Scripts = Scripts
  { scriptsOneShotPolicy :: !(TypedScript 'MintingPolicyRole '[AsData TxOutRef])
  , scriptCarbonPolicy :: !(TypedScript 'MintingPolicyRole '[TxId, Integer, TokenName])
  , scriptMintingNftPolicy :: !(TypedScript 'MintingPolicyRole '[TxOutRef])
  , scriptMarketplaceValidator :: !(TypedScript 'ValidatorRole '[ScriptHash, ScriptHash])
  , scriptOracleMintingPolicy :: !(TypedScript 'MintingPolicyRole '[AssetClass, PubKeyHash])
  }

oneShotMintingPolicy :: GYTxOutRef -> Scripts -> GYMintingPolicy 'PlutusV2
oneShotMintingPolicy oref scripts =
  mintingPolicyFromPly $
    scriptsOneShotPolicy scripts
      # (AsData . txOutRefToPlutus $ oref)
