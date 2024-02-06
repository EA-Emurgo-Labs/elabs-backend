{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Marketplace (MarketplaceAction (..), MarketplaceParams (..), MarketplaceDatum (..)) where

import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

data MarketplaceAction
  = BUY
  | WITHDRAW
  | SELL {newPrice :: Integer}
  | CANCELL
  | BUY_PARTIAL {amt :: Integer}

PlutusTx.makeIsDataIndexed ''MarketplaceAction [('BUY, 0), ('WITHDRAW, 1), ('SELL, 2), ('CANCELL, 3), ('BUY_PARTIAL, 4)]

data MarketplaceParams = MarketplaceParams
  { mktpOracleValidator :: ScriptHash
  , mktpEscrowValidator :: ScriptHash
  }

PlutusTx.unstableMakeIsData ''MarketplaceParams

data MarketplaceDatum = MarketplaceDatum
  { mktDtmOwner :: PubKeyHash
  , mktDtmSalePrice :: PlutusTx.Integer
  , mktDtmAssetSymbol :: CurrencySymbol
  , mktDtmAssetName :: TokenName
  , mktDtmAmount :: PlutusTx.Integer
  }
