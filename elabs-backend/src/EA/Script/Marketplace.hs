{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Marketplace (MarketplaceAction (..), MarketplaceScriptParams (..), MarketplaceDatum (..), MarketplaceParams (..), marketPlaceParamsToScriptParams) where

import GeniusYield.Types (GYMintingPolicyId, GYTokenName, GYValidatorHash, mintingPolicyIdCurrencySymbol, tokenNameToPlutus, validatorHashToPlutus)
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

data MarketplaceScriptParams = MarketplaceScriptParams
  { mktSpOracleValidator :: ScriptHash
  , mktSpEscrowValidator :: ScriptHash
  , mktSpVersion :: TokenName
  , mktSpOracleSymbol :: CurrencySymbol
  , mktSpOracleTokenName :: TokenName
  }

PlutusTx.unstableMakeIsData ''MarketplaceScriptParams

data MarketplaceDatum = MarketplaceDatum
  { mktDtmOwner :: PubKeyHash
  , mktDtmSalePrice :: PlutusTx.Integer
  , mktDtmAssetSymbol :: CurrencySymbol
  , mktDtmAssetName :: TokenName
  , mktDtmAmount :: PlutusTx.Integer
  , mktDtmIssuer :: PubKeyHash
  , mktDtmIsSell :: PlutusTx.Integer
  }

PlutusTx.unstableMakeIsData ''MarketplaceDatum

data MarketplaceParams = MarketplaceParams
  { mktPrmOracleValidator :: GYValidatorHash
  , mktPrmEscrowValidator :: GYValidatorHash
  , mktPrmVersion :: GYTokenName
  , mktPrmOracleSymbol :: GYMintingPolicyId
  , mktPrmOracleTokenName :: GYTokenName
  }

marketPlaceParamsToScriptParams :: MarketplaceParams -> MarketplaceScriptParams
marketPlaceParamsToScriptParams MarketplaceParams {..} =
  MarketplaceScriptParams
    { mktSpOracleValidator = validatorHashToPlutus mktPrmOracleValidator
    , mktSpEscrowValidator = validatorHashToPlutus mktPrmEscrowValidator
    , mktSpVersion = tokenNameToPlutus mktPrmVersion
    , mktSpOracleSymbol = mintingPolicyIdCurrencySymbol mktPrmOracleSymbol
    , mktSpOracleTokenName = tokenNameToPlutus mktPrmOracleTokenName
    }
