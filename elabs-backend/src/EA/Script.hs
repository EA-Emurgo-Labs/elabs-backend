{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module EA.Script (Scripts (..)) where

import PlutusLedgerApi.V2 (TxOutRef)
import Ply (AsData, ScriptRole (MintingPolicyRole), TypedScript)

data Scripts = Scripts
  { scriptsOneShotPolicy :: !(TypedScript 'MintingPolicyRole '[AsData TxOutRef])
  }
