module EA.Tx.Changeblock.Marketplace (
  MarketplaceInfo (..),
  buy,
) where

import EA ()
import EA.Script (Scripts, marketplaceValidator)
import EA.Script.Marketplace (
  MarketplaceDatum (..),
  MarketplaceInfo (..),
  MarketplaceParams (..),
  marketplaceInfoToDatum,
 )
import EA.Script.Marketplace qualified as MarketplaceAction
import EA.Script.Oracle (OracleInfo (..))
import GeniusYield.TxBuilder (
  GYTxSkeleton,
  mustBeSignedBy,
  mustHaveInput,
  mustHaveOutput,
  mustHaveRefInput,
 )
import GeniusYield.Types (
  GYAssetClass (GYToken),
  GYInScript (GYInReference, GYInScript),
  GYNetworkId,
  GYPubKeyHash,
  GYTxIn (GYTxIn, gyTxInTxOutRef, gyTxInWitness),
  GYTxInWitness (GYTxInWitnessScript),
  GYTxOutRef,
  PlutusVersion (PlutusV2),
  addressFromPubKeyHash,
  addressFromValidator,
  datumFromPlutusData,
  mintingPolicyIdCurrencySymbol,
  mkGYTxOut,
  mkGYTxOutNoDatum,
  pubKeyHashToPlutus,
  redeemerFromPlutusData,
  tokenNameToPlutus,
  validatorToScript,
  valueFromLovelace,
  valueSingleton,
 )

type BuyerPubkeyHash = GYPubKeyHash

buy :: GYNetworkId -> MarketplaceInfo -> OracleInfo -> BuyerPubkeyHash -> Maybe GYTxOutRef -> MarketplaceParams -> Scripts -> GYTxSkeleton 'PlutusV2
buy nid info@MarketplaceInfo {..} OracleInfo {..} buyerPubKeyHash mMarketplaceRefScript mktPlaceParams scripts =
  let mktPlaceValidator = marketplaceValidator mktPlaceParams scripts
      marketplaceAddr = addressFromValidator nid mktPlaceValidator
      marketOutValue = valueSingleton (GYToken mktInfoCarbonPolicyId mktInfoCarbonAssetName) mktInfoAmount
      escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktPlaceParams)
      ownerAddress = addressFromPubKeyHash nid mktInfoOwner
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (input mktPlaceValidator)
        <> mustHaveOutput (mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * mktInfoAmount)))
        <> mustHaveOutput (mkGYTxOut marketplaceAddr marketOutValue (datumFromPlutusData newDatum))
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustBeSignedBy buyerPubKeyHash
  where
    input validator =
      GYTxIn
        { gyTxInTxOutRef = mktInfoTxOutRef
        , gyTxInWitness =
            GYTxInWitnessScript
              (inputWitness validator)
              (datumFromPlutusData $ marketplaceInfoToDatum info)
              (redeemerFromPlutusData MarketplaceAction.BUY)
        }
    inputWitness validator = maybe (GYInScript validator) (\ref -> GYInReference ref (validatorToScript validator)) mMarketplaceRefScript

    newDatum =
      MarketplaceDatum
        { mktDtmOwner = pubKeyHashToPlutus buyerPubKeyHash
        , mktDtmSalePrice = mktInfoSalePrice
        , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
        , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
        , mktDtmAmount = mktInfoAmount
        , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
        , mktDtmIsSell = 0
        }
