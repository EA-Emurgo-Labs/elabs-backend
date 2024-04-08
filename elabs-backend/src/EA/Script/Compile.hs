module EA.Script.Compile where

import PlutusTx.Prelude qualified as PlutusTx 

-- | I need a way to construct a parameter type to use 
type Params 
  = EScriptParams
  | FracScriptParams
  | NFTScriptParams
  | CounterScriptParams

-- etc. So i can easily use any defined parameter set on any defined script that requires parameters

-- | Generates MintingPolicy given params.
mkMintingValidator :: Params -> PlutusTx.CompiledCode -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
mintingValidator params script =
    $$ script `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

-- | Generates validator given params.
mkSpendingValidator :: Params -> PlutusTx.CompiledCode -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
spendingValidator params script =
    $$ script `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params