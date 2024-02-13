module EA.Tx.Changeblock.Operation (mintIpfsNftCarbonToken) where

import EA.Script (Scripts (..), carbonMintingPolicy, marketplaceValidator, nftMintingPolicy)
import EA.Script.Marketplace (MarketplaceDatum (..), MarketplaceParams)
import GeniusYield.TxBuilder (GYTxSkeleton, mustHaveInput, mustHaveOutput, mustMint, scriptAddress)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad)
import GeniusYield.Types

type NftTokenName = GYTokenName

type CarbonTokenName = GYTokenName

type MarketPlaceSalePrice = Integer

mintIpfsNftCarbonToken ::
  (GYTxQueryMonad m) =>
  -- | The UTXO to consume
  GYTxOutRef ->
  -- | The Marketplace Param
  MarketplaceParams ->
  -- | Owner of Marketplace
  GYPubKeyHash ->
  -- | TokenName generated with combination of IPFS hash
  NftTokenName ->
  -- | TokenName For Carbon Minting policy
  CarbonTokenName ->
  -- | Marketplace Sale Price
  MarketPlaceSalePrice ->
  -- | Amount of token to mint
  Integer ->
  -- | The EA Scripts
  Scripts ->
  m (GYTxSkeleton 'PlutusV2)
mintIpfsNftCarbonToken oref mktParams pkh nftTokenName carbonTokenName mktSalePrice qty script = do
  let carbonPolicy = carbonMintingPolicy oref nftTokenName script
      outMarketplaceToken = GYToken (mintingPolicyId carbonPolicy) nftTokenName
      outMarketplaceValue = valueSingleton outMarketplaceToken qty
      nftPolicy = nftMintingPolicy oref script

      marketPlaceDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus pkh
          , mktDtmIssuer = pubKeyHashToPlutus pkh
          , mktDtmIsSell = 1
          , mktDtmSalePrice = mktSalePrice
          , mktDtmAssetSymbol = mintingPolicyCurrencySymbol carbonPolicy
          , mktDtmAssetName = tokenNameToPlutus nftTokenName
          , mktDtmAmount = qty
          }
  marketPlaceAddr <- scriptAddress $ marketplaceValidator mktParams script

  return $
    mustMint (GYMintScript nftPolicy) unitRedeemer nftTokenName qty
      <> mustMint (GYMintScript carbonPolicy) unitRedeemer carbonTokenName qty
      <> mustHaveInput (GYTxIn oref GYTxInWitnessKey)
      <> mustHaveOutput (mkGYTxOut marketPlaceAddr outMarketplaceValue (datumFromPlutusData marketPlaceDatum))
