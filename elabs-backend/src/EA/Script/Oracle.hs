{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Oracle (OracleDatum (..), OracleScriptParams (..), OracleAction (..), oracleMintingPolicy) where

import EA.Script (Scripts (..))
import GeniusYield.Types
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

data OracleDatum = OracleDatum
  { orcDtmRate :: PlutusTx.Integer
  }

PlutusTx.unstableMakeIsData ''OracleDatum

data OracleScriptParams = OracleScriptParams
  { orcScriptPrmNft :: AssetClass
  , orcScriptPrmOperator :: PubKeyHash
  }

PlutusTx.unstableMakeIsData ''OracleParams

data OracleAction
  = Update
  | Delete

PlutusTx.makeIsDataIndexed ''OracleAction [('Update, 0), ('Delete, 1)]
