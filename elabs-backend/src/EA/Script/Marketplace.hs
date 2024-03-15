{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Marketplace (
  MarketplaceAction (..),
  MarketplaceScriptParams (..),
  MarketplaceDatum (..),
  MarketplaceParams (..),
  MarketplaceInfo (..),
  MarketplaceOrderType (..),
  marketPlaceParamsToScriptParams,
  marketplaceInfoToDatum,
  marketplaceDatumToInfo,
) where

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (assetClass)
import PlutusLedgerApi.V2 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

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
  , mktSpBackdoor :: PubKeyHash
  -- ^ The PubKeyHash of Backdoor
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
  , mktPrmBackdoor :: GYPubKeyHash
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
    , mktSpBackdoor = pubKeyHashToPlutus mktPrmBackdoor
    }

data MarketplaceOrderType = M_BUY | M_SELL
  deriving stock (Enum, Show, Eq, Generic)
  deriving anyclass (Swagger.ToSchema, Swagger.ToParamSchema)

instance Aeson.FromJSON MarketplaceOrderType where
  parseJSON = Aeson.withText "MarketplaceOrderType" $ \case
    "BUY" -> pure M_BUY
    "SELL" -> pure M_SELL
    _ -> fail "Invalid MarketplaceOrderType: "

instance Aeson.ToJSON MarketplaceOrderType where
  toJSON M_BUY = Aeson.String "BUY"
  toJSON M_SELL = Aeson.String "SELL"

instance FromHttpApiData MarketplaceOrderType where
  parseUrlPiece t = either (Left . T.pack) Right $ Aeson.eitherDecode $ Aeson.encode t

instance ToHttpApiData MarketplaceOrderType where
  toUrlPiece M_BUY = "BUY"
  toUrlPiece M_SELL = "SELL"

data MarketplaceInfo = MarketplaceInfo
  { mktInfoTxOutRef :: GYTxOutRef
  , mktInfoAddress :: GYAddress
  , mktInfoValue :: GYValue
  , mktInfoOwner :: GYPubKeyHash
  , mktInfoSalePrice :: Integer
  , mktInfoCarbonPolicyId :: GYMintingPolicyId
  , mktInfoCarbonAssetName :: GYTokenName
  , mktInfoAmount :: Integer
  , mktInfoIssuer :: GYPubKeyHash
  , mktInfoIsSell :: MarketplaceOrderType
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON MarketplaceInfo where
  toJSON MarketplaceInfo {..} =
    Aeson.object
      [ "tx_ref" Aeson..= mktInfoTxOutRef
      , "address" Aeson..= mktInfoAddress
      , "value" Aeson..= mktInfoValue
      , "owner" Aeson..= mktInfoOwner
      , "price" Aeson..= mktInfoSalePrice
      , "carbon-token-id" Aeson..= mktInfoCarbonPolicyId
      , "carbon-token-name" Aeson..= mktInfoCarbonAssetName
      , "amount" Aeson..= mktInfoAmount
      , "issuer" Aeson..= mktInfoIssuer
      , "order_type" Aeson..= mktInfoIsSell
      ]

instance Swagger.ToSchema MarketplaceInfo where
  declareNamedSchema _ = do
    txOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    addressSchema <- Swagger.declareSchemaRef @GYAddress Proxy
    valueSchema <- Swagger.declareSchemaRef @GYValue Proxy
    pubKeyHashSchema <- Swagger.declareSchemaRef @GYPubKeyHash Proxy
    mintingPolicyIdSchema <- Swagger.declareSchemaRef @GYMintingPolicyId Proxy
    tokenNameSchema <- Swagger.declareSchemaRef @GYTokenName Proxy
    orderTypeSchema <- Swagger.declareSchemaRef @MarketplaceOrderType Proxy
    integerSchema <- Swagger.declareSchemaRef @Integer Proxy
    return $
      Swagger.named "MarketplaceInfo" $
        mempty
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack "tx_ref", txOutRefSchema)
               , (T.pack "address", addressSchema)
               , (T.pack "value", valueSchema)
               , (T.pack "owner", pubKeyHashSchema)
               , (T.pack "price", integerSchema)
               , (T.pack "carbon-token-id", mintingPolicyIdSchema)
               , (T.pack "carbon-token-name", tokenNameSchema)
               , (T.pack "amount", integerSchema)
               , (T.pack "issuer", pubKeyHashSchema)
               , (T.pack "order_type", orderTypeSchema)
               ]
          & Swagger.required .~ [T.pack "tx_ref", T.pack "address", T.pack "value", T.pack "owner", T.pack "price", T.pack "carbon-token-id", T.pack "carbon-token-name", T.pack "amount", T.pack "issuer", T.pack "order_type"]
          & Swagger.description ?~ "Marketplace Order Info"
          & Swagger.maxProperties ?~ 10
          & Swagger.minProperties ?~ 10

marketplaceInfoToDatum :: MarketplaceInfo -> MarketplaceDatum
marketplaceInfoToDatum MarketplaceInfo {..} =
  MarketplaceDatum
    { mktDtmOwner = pubKeyHashToPlutus mktInfoOwner
    , mktDtmSalePrice = mktInfoSalePrice
    , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
    , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
    , mktDtmAmount = mktInfoAmount
    , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
    , mktDtmIsSell = toInteger $ fromEnum mktInfoIsSell
    }

marketplaceDatumToInfo ::
  GYTxOutRef ->
  GYValue ->
  GYAddress ->
  MarketplaceDatum ->
  Either String MarketplaceInfo
marketplaceDatumToInfo oref val addr datum = do
  pubkeyIssuer <- seither . pubKeyHashFromPlutus $ mktDtmIssuer datum
  pubkeyOwner <- seither . pubKeyHashFromPlutus $ mktDtmOwner datum
  (tokenPolicy, tokenName) <-
    seither $
      unpackAc
        <$> assetClassFromPlutus
          (assetClass (mktDtmAssetSymbol datum) (mktDtmAssetName datum))

  return
    MarketplaceInfo
      { mktInfoTxOutRef = oref
      , mktInfoAddress = addr
      , mktInfoValue = val
      , mktInfoOwner = pubkeyOwner
      , mktInfoSalePrice = mktDtmSalePrice datum
      , mktInfoCarbonPolicyId = tokenPolicy
      , mktInfoCarbonAssetName = tokenName
      , mktInfoAmount = mktDtmAmount datum
      , mktInfoIssuer = pubkeyIssuer
      , mktInfoIsSell = toEnum $ fromInteger $ mktDtmIsSell datum
      }
  where
    seither :: (Show b) => Either b a -> Either String a
    seither = either (Left . show) Right

    unpackAc :: GYAssetClass -> (GYMintingPolicyId, GYTokenName)
    unpackAc (GYToken tokenPolicy tokenName) = (tokenPolicy, tokenName)
    unpackAc _ = ("", "")
