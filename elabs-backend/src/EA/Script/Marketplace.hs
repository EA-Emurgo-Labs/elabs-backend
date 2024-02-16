{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Marketplace (MarketplaceAction (..), MarketplaceScriptParams (..), MarketplaceDatum (..), MarketplaceParams (..), marketPlaceParamsToScriptParams) where

import GeniusYield.Types (GYMintingPolicyId, GYPubKeyHash, GYTokenName, GYValidatorHash, mintingPolicyIdCurrencySymbol, pubKeyHashToPlutus, tokenNameToPlutus, validatorHashToPlutus)
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

data MarketplaceAction
  = BUY
  | MERGE
  | SELL {newPrice :: Integer}
  | CANCEL
  | BUY_PARTIAL {amt :: Integer}
  deriving stock (Show)

PlutusTx.makeIsDataIndexed ''MarketplaceAction [('BUY, 0), ('MERGE, 1), ('SELL, 2), ('CANCEL, 3), ('BUY_PARTIAL, 4)]

data MarketplaceScriptParams = MarketplaceScriptParams
  { mktSpOracleValidator :: ScriptHash
  -- ^ The script Hash of Oracle Validator
  , mktSpEscrowValidator :: PubKeyHash
  -- ^ The PubKeyHash of Escrow. Fee will be sent to this address
  , mktSpVersion :: TokenName
  -- ^ This is just used to make different version of Marketplace while testing.
  , mktSpOracleSymbol :: CurrencySymbol
  -- ^ The policyId of Oracle Token
  , mktSpOracleTokenName :: TokenName
  -- ^ The TokenName of Oracle Token
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''MarketplaceScriptParams

data MarketplaceDatum = MarketplaceDatum
  { mktDtmOwner :: PubKeyHash
  -- ^ The owner of the Order
  , mktDtmSalePrice :: PlutusTx.Integer
  -- ^ The Sale Price
  , mktDtmAssetSymbol :: CurrencySymbol
  -- ^ The policyId of Carbon Token
  , mktDtmAssetName :: TokenName
  -- ^ The TokenName of Carbon Token
  , mktDtmAmount :: PlutusTx.Integer
  -- ^ The Amount of Carbon Token
  , mktDtmIssuer :: PubKeyHash
  -- ^ The issuer of the Order. One who minted the token
  , mktDtmIsSell :: PlutusTx.Integer
  -- ^ The type of Order. 1 if is OnSell else 0
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

data MarketplaceParams = MarketplaceParams
  { mktPrmOracleValidator :: GYValidatorHash
  , mktPrmEscrowValidator :: GYPubKeyHash
  , mktPrmVersion :: GYTokenName
  , mktPrmOracleSymbol :: GYMintingPolicyId
  , mktPrmOracleTokenName :: GYTokenName
  }
  deriving stock (Show)

marketPlaceParamsToScriptParams :: MarketplaceParams -> MarketplaceScriptParams
marketPlaceParamsToScriptParams MarketplaceParams {..} =
  MarketplaceScriptParams
    { mktSpOracleValidator = validatorHashToPlutus mktPrmOracleValidator
    , mktSpEscrowValidator = pubKeyHashToPlutus mktPrmEscrowValidator
    , mktSpVersion = tokenNameToPlutus mktPrmVersion
    , mktSpOracleSymbol = mintingPolicyIdCurrencySymbol mktPrmOracleSymbol
    , mktSpOracleTokenName = tokenNameToPlutus mktPrmOracleTokenName
    }
