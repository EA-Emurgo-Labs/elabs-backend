module EA.Script (Scripts (..), oneShotMintingPolicy, carbonMintingPolicy, nftMintingPolicy, marketplaceValidator, oracleMintingPolicy) where

import EA.Internal (mintingPolicyFromPly, validatorFromPly)
import EA.Script.Marketplace (MarketplaceParams, MarketplaceScriptParams (..), marketPlaceParamsToScriptParams)
import GeniusYield.Types
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName, TxOutRef)
import PlutusLedgerApi.V1.Tx (TxId)
import PlutusLedgerApi.V1.Value (AssetClass)
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
  , scriptMarketplaceValidator :: !(TypedScript 'ValidatorRole '[ScriptHash, ScriptHash, TokenName, CurrencySymbol, TokenName])
  , scriptOracleMintingPolicy :: !(TypedScript 'MintingPolicyRole '[AssetClass, PubKeyHash])
  }

oneShotMintingPolicy :: GYTxOutRef -> Scripts -> GYMintingPolicy 'PlutusV2
oneShotMintingPolicy oref scripts =
  mintingPolicyFromPly $
    scriptsOneShotPolicy scripts
      # (AsData . txOutRefToPlutus $ oref)

carbonMintingPolicy :: GYTxOutRef -> GYTokenName -> Scripts -> GYMintingPolicy 'PlutusV2
carbonMintingPolicy oref tn scripts =
  mintingPolicyFromPly $
    scriptCarbonPolicy scripts
      # txId
      # txIndx
      # tokenNameToPlutus tn
  where
    txId = show . fst . txOutRefToTuple $ oref
    txIndx = fromIntegral . snd . txOutRefToTuple $ oref

nftMintingPolicy :: GYTxOutRef -> Scripts -> GYMintingPolicy 'PlutusV2
nftMintingPolicy oref scripts =
  mintingPolicyFromPly $
    scriptMintingNftPolicy scripts
      # txOutRefToPlutus oref

marketplaceValidator :: MarketplaceParams -> Scripts -> GYValidator 'PlutusV2
marketplaceValidator mktParam scripts =
  let MarketplaceScriptParams {..} = marketPlaceParamsToScriptParams mktParam
   in validatorFromPly $
        scriptMarketplaceValidator scripts
          # mktSpOracleValidator
          # mktSpEscrowValidator
          # mktSpVersion
          # mktSpOracleSymbol
          # mktSpOracleTokenName

oracleMintingPolicy :: GYAssetClass -> GYPubKeyHash -> Scripts -> GYMintingPolicy 'PlutusV2
oracleMintingPolicy asset pHash scripts =
  mintingPolicyFromPly $
    scriptOracleMintingPolicy scripts
      # assetClassToPlutus asset
      # pubKeyHashToPlutus pHash
