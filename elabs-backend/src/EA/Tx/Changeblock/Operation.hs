module EA.Tx.Changeblock.Operation (mintIpfsNftCarbonToken) where

import EA.Script (Scripts (..), carbonMintingPolicy, marketplaceValidator, nftPolicy)
import EA.Script.Marketplace (MarketplaceDatum (..), MarketplaceParams)
import GeniusYield.TxBuilder (GYTxSkeleton, mustHaveInput, mustHaveOutput, mustMint, scriptAddress)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad)
import GeniusYield.Types

type RandomIpfsTokenName = GYTokenName

type MarketPlaceSalePrice = Integer

mintIpfsNftCarbonToken ::
  GYTxQueryMonad m =>
  GYTxOutRef ->
  MarketplaceParams ->
  -- | Owner of Marketplace
  GYPubKeyHash ->
  -- | Random IPFS Token Name
  RandomIpfsTokenName ->
  -- | Marketplace Sale Price
  MarketPlaceSalePrice ->
  -- | Amount of token to mint
  Integer ->
  -- | The EA Scripts
  Scripts ->
  m (GYTxSkeleton 'PlutusV2)
mintIpfsNftCarbonToken oref mktParams pkh rndipfsTknName mktSalePrice qty script = do
  let marketPlaceDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus pkh,
            mktDtmIssuer = pubKeyHashToPlutus pkh,
            mktDtmIsSell = 1,
            mktDtmSalePrice = mktSalePrice,
            mktDtmAssetSymbol = mintingPolicyCurrencySymbol finalCarbonPolicy,
            mktDtmAssetName = tokenNameToPlutus rndipfsTknName,
            mktDtmAmount = qty
          }

  marketPlaceAddr <- scriptAddress $ marketplaceValidator mktParams script
  let outMarketplaceToken = GYToken (mintingPolicyId finalCarbonPolicy) rndipfsTknName
      outMarketplaceValue = valueSingleton outMarketplaceToken qty

  return $
    mustMint (GYMintScript finalNftPolicy) unitRedeemer rndipfsTknName qty
      <> mustMint (GYMintScript finalCarbonPolicy) unitRedeemer carbonTokenName qty
      <> mustHaveInput (GYTxIn oref GYTxInWitnessKey)
      <> mustHaveOutput (mkGYTxOut marketPlaceAddr outMarketplaceValue (datumFromPlutusData marketPlaceDatum))
  where
    carbonTokenName = "CBLK"
    finalNftPolicy = nftPolicy oref script
    finalCarbonPolicy = carbonMintingPolicy oref rndipfsTknName script