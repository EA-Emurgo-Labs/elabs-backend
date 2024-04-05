module EA.EScript (
  Scripts (..),
  eMintingPolicy,
  eSpendingValidator
) where 

import EA.Script.Enum (
  EParams,
  EScriptParams (..),
  eParamsToScriptParams,
  mkSpendingValidator,
 )
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  PubKeyHash,
  ScriptHash,
  TokenName,
  TxOutRef,
 )

data Scripts = Scripts 
  { scriptEMintingPolicy :: Validator
  , scriptESpendingValidator :: 
  }

eMintingPolicy :: Scripts -> GYMintingPolicy 'PlutusV2 
eMintingPolicy scripts = validatorFromPlutus $ 
  scriptEMintingPolicy scripts

eSpendingValidator :: EScriptParams -> Scripts -> GYMintingPolicy 'PlutusV2
eSpendingValidator params scripts = validatorFromPlutus $
  mkSpendingValidator params (scriptESpendingValidator scripts)

