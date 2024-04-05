{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Enum (
  EAction (..),
  EDatum (..),
  EScriptParams (..),
  EParams (..),
  EMintInfo (..),
  ESpendInfo (..),
  EActionType (..),
  eParamsToScriptParams,
  eInfoToDatum,
  eDatumToInfo,
  mk
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

-- Define Plutus & Atlas Data for Enum Scripts

-- | Generates MintingPolicy given params. 
-- NOTE: This is only needed to apply params - we wont need it in this example
mkMintingValidator :: _ -> PlutusTx.CompiledCode -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
mintingValidator params script =
    $$ script `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

-- | Generates validator given params.
mkSpendingValidator :: EScriptParams -> PlutusTx.CompiledCode -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
spendingValidator params script =
    $$ script `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

data EAction -- Redeemer for both validators
  = Alpha
  | Beta
  deriving stock (Show)

PlutusTx.makeIsDataIndexed ''EAction [('Alpha, 0), ('Beta, 1)]

data EDatum = EDatum 
  { a :: PlutusTx.Integer
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''EDatum 

data EScriptParams = EScriptParams -- Plutus Params for EVal
  { p :: CurrencySymbol
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''EScriptParams

data EParams = EParams -- GYParams
  { p :: GYMintingPolicyId
  }
  deriving stock (Show)

eParamsToScriptParams :: EParams -> EScriptParams 
eParamsToScriptParams EParams {..} =
  EScriptParams
    { p = mintingPolicyIdCurrencySymbol p 
    }

data EActionType = E_Alpha | E_Beta
  deriving stock (Enum, Show, Eq, Generic)
  deriving anyclass (Swagger.ToSchema, Swagger.ToParamSchema)

instance Aeson.FromJSON EActionType where 
  parseJSON = Aeson.withText "EActionType" $ \case
    "Alpha" -> pure E_Alpha
    "Beta" -> pure E_Beta
    _ -> fail "Invalid EActionType: "

instance Aeson.ToJSON EActionType where
  toJSON E_Alpha = Aeson.String "Alpha"
  toJSON E_Beta = Aeson.String "Beta"

instance FromHttpApiData MarketplaceOrderType where
  parseUrlPiece t = either (Left . T.pack) Right $ Aeson.eitherDecode $ Aeson.encode t

instance ToHttpApiData EActionType where
  toUrlPiece E_Alpha = "Alpha"
  toUrlPiece E_Beta = "Beta"

data EMintInfo = EMintInfo -- for EMint
  { eInfoAddress :: GYAddress
  , eInfoPolicyId :: GYMintingPolicyId
  , eInfoTokenName :: GYTokenName
  , eInfoAmt :: Integer
  , eInfoDatum :: Integer 
  , eInfoAction :: EActionType 
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON EMintInfo where 
  toJSON MarketplaceInfo {..} =
    Aeson.object
      [ "address" Aeson..= eInfoAddress
      , "policy-id" Aeson..= eInfoPolicyId
      , "token-name" Aeson..= eInfoTokenName
      , "amount" Aeson..= eInfoAmt
      , "datum-no" Aeson..= eInfoDatum
      , "action_type" Aeson..= eInfoAction
      ]

instance Swagger.ToSchema EMintInfo where 
  declareNamedSchema _ = do 
    addressSchema <- Swagger.declareSchemaRef @GYAddress Proxy 
    policyIdSchema <- Swagger.declareSchemaRef @GYMintingPolicyId Proxy
    tokenNameSchema <- Swagger.declareSchemaRef @GYTokenName Proxy
    tokenAmountSchema <- Swagger.declareSchemaRef @Integer Proxy
    datumNumberSchema <- Swagger.declareSchemaRef @Integer Proxy
    actionTypeSchema <- Swagger.declareSchemaRef @EActionType Proxy
    return $ 
      Swagger.named "EMintInfo" $ 
        mempty 
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack "address", addressSchema)
               , (T.pack "policy-id", policyIdSchema)
               , (T.pack "token-name", tokenNameSchema)
               , (T.pack "amount", tokenAmountSchema)
               , (T.pack "datum-no", datumNumberSchema)
               , (T.pack "action_type", actionTypeSchema)
               ]
          & Swagger.required .~ [T.pack "address", T.pack "policy-id", T.pack "token-name", T.pack "amount", T.pack "datum-no", T.pack "action_type"]
          & Swagger.description ?~ "Enum Minting Info"
          & Swagger.maxProperties ?~ 6
          & Swagger.minProperties ?~ 6

data ESpendInfo = ESpendInfo -- for EVal
  { eInfoTxOutRef :: GYTxOutRef 
  , eInfoAddress :: GYAddress 
  , eInfoPolicyId :: GYMintingPolicyId
  , eInfoTokenName :: GYTokenName
  , eInfoAmt :: Integer
  , eInfoDatum :: Integer 
  , eInfoAction :: EActionType
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON ESpendInfo where 
  toJSON MarketplaceInfo {..} =
    Aeson.object
      [ "tx_ref" Aeson..= eInfoTxOutRef
      , "address" Aeson..= eInfoAddress
      , "policy-id" Aeson..= eInfoPolicyId
      , "token-name" Aeson..= eInfoTokenName
      , "amount" Aeson..= eInfoAmt
      , "datum-no" Aeson..= eInfoDatum
      , "action_type" Aeson..= eInfoAction
      ]

instance Swagger.ToSchema ESpendInfo where 
  declareNamedSchema _ = do 
    txOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    addressSchema <- Swagger.declareSchemaRef @GYAddress Proxy 
    policyIdSchema <- Swagger.declareSchemaRef @GYMintingPolicyId Proxy
    tokenNameSchema <- Swagger.declareSchemaRef @GYTokenName Proxy
    tokenAmountSchema <- Swagger.declareSchemaRef @Integer Proxy
    datumNumberSchema <- Swagger.declareSchemaRef @Integer Proxy
    actionTypeSchema <- Swagger.declareSchemaRef @EActionType Proxy
    return $ 
      Swagger.named "ESpendInfo" $ 
        mempty 
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack "tx_ref", txOutRefSchema)
               , (T.pack "address", addressSchema)
               , (T.pack "policy-id", policyIdSchema)
               , (T.pack "token-name", tokenNameSchema)
               , (T.pack "amount", tokenAmountSchema)
               , (T.pack "datum-no", datumNumberSchema)
               , (T.pack "action_type", actionTypeSchema)
               ]
          & Swagger.required .~ [T.pack "address", T.pack "policy-id", T.pack "token-name", T.pack "amount", T.pack "datum-no", T.pack "action_type"]
          & Swagger.description ?~ "Enum Spending Info"
          & Swagger.maxProperties ?~ 7
          & Swagger.minProperties ?~ 7

emintInfoToDatum :: EMintInfo -> EDatum 
emintInfoToDatum EMintInfo {..} =
  EDatum { a = eInfoDatum }

emintDatumToInfo :: 
  GYAddress -> 
  GYMintingPolicyId ->
  GYTokenName ->
  Integer ->
  EDatum -> 
  Either String EMintInfo 
emintDatumToInfo addr policyId tokenName action datum = do 
  
  return
    EMintInfo 
      { eInfoAddress = addr
      , eInfoPolicyId = policyId
      , eInfoTokenName = tokenName
      , eInfoAmt = 1
      , eInfoDatum = a datum 
      , eInfoAction = toEnum $ fromInteger $ action
      }

espendInfoToDatum :: ESpendInfo -> EDatum 
emintInfoToDatum ESpendInfo {..} =
  EDatum { a = eInfoDatum }

emintDatumToInfo :: 
  GYTxOutRef ->
  GYAddress -> 
  GYMintingPolicyId ->
  GYTokenName ->
  Integer ->
  EDatum -> 
  Either String EMintInfo 
emintDatumToInfo oref addr policyId tokenName action datum = do 
  
  return
    EMintInfo 
      { eInfoTxOutRef = oref 
      , eInfoAddress = addr
      , eInfoPolicyId = policyId
      , eInfoTokenName = tokenName
      , eInfoAmt = 1
      , eInfoDatum = a datum 
      , eInfoAction = toEnum $ fromInteger $ action
      }