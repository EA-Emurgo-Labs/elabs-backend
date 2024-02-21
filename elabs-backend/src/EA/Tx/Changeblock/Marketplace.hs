module EA.Tx.Changeblock.Marketplace (MarketplaceInfo (..), buy, marketplaceAtTxOutRef) where

import EA.Script (Scripts, marketplaceValidator)
import EA.Script.Marketplace (MarketplaceDatum (..), MarketplaceInfo (..), MarketplaceParams (..), marketplaceInfoToDatum, marketplaceDatumToInfo)
import EA.Script.Marketplace qualified as MarketplaceAction
import EA.Script.Oracle (OracleInfo (..))
import GeniusYield.TxBuilder (GYTxSkeleton, mustBeSignedBy, mustHaveInput, mustHaveOutput, mustHaveRefInput, utxoDatumPure)
import GeniusYield.Types
import EA

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

marketplaceAtTxOutRef :: GYTxOutRef -> EAApp MarketplaceInfo
marketplaceAtTxOutRef oref = do
    providers <- asks eaAppEnvGYProviders
    utxos <- liftIO $ gyQueryUtxosAtTxOutRefsWithDatums providers [oref]
    utxo <- eaLiftMaybe "No UTXO found" $ listToMaybe utxos
    (addr, val, datum) <-
      eaLiftEither (const "Cannot extract data from UTXO")
        $ utxoDatumPure @MarketplaceDatum utxo

    return $ marketplaceDatumToInfo oref val addr datum

_marketplaceInfos :: GYNetworkId -> MarketplaceParams -> Scripts -> EAApp [MarketplaceInfo]
_marketplaceInfos nid mktPlaceParams scripts = do
  let mktPlaceValidator = marketplaceValidator mktPlaceParams scripts
      marketplaceAddr = addressFromValidator nid mktPlaceValidator
  providers <- asks eaAppEnvGYProviders
  _utxos <- liftIO $ gyQueryUtxosAtAddressesWithDatums providers [marketplaceAddr]
  let _utxosRight = filter(\x -> isRight . utxoDatumPure @MarketplaceDatum x) _utxos
  --Todo check each utxo
  return $ map (\x ->  do
        (addr, val, datum) <- eaLiftEither (const "Cannot extract data from UTXO") $ utxoDatumPure @MarketplaceDatum x
         marketplaceDatumToInfo (first $ utxosRefs $ utxosFromUTxO x) val addr datum
        ) _utxosRight