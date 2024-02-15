module EA.Tx.Changeblock.Operation (mintIpfsNftCarbonToken) where

import EA.Script (Scripts (..), carbonNftMintingPolicy, carbonTokenMintingPolicy, marketplaceValidator)
import EA.Script.Marketplace (MarketplaceDatum (..), MarketplaceParams)
import GeniusYield.TxBuilder (GYTxSkeleton, mustHaveInput, mustHaveOutput, mustMint, scriptAddress)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad)
import GeniusYield.Types

type MarketPlaceSalePrice = Integer

mintIpfsNftCarbonToken ::
  (GYTxQueryMonad m) =>
  -- | The UTXO to consume
  GYTxOutRef ->
  -- | The Marketplace Param
  MarketplaceParams ->
  -- | Owner Address
  GYAddress ->
  -- | Owner Pubkey hash
  GYPubKeyHash ->
  -- | TokenName generated with combination of IPFS hash
  GYTokenName ->
  -- | Marketplace Sale Price
  MarketPlaceSalePrice ->
  -- | Amount of token to mint
  Integer ->
  -- | The EA Scripts
  Scripts ->
  m (GYTxSkeleton 'PlutusV2)
mintIpfsNftCarbonToken oref mktParams ownerAddr pkh tn mktSalePrice qty script = do
  let carbonNftPolicy = carbonNftMintingPolicy oref tn script
      carbonTokenPolicy = carbonTokenMintingPolicy tn script
      carbonToken = GYToken (mintingPolicyId carbonTokenPolicy) tn
      carbonNftToken = GYToken (mintingPolicyId carbonNftPolicy) tn

      marketPlaceDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus pkh,
            mktDtmIssuer = pubKeyHashToPlutus pkh,
            mktDtmIsSell = 1,
            mktDtmSalePrice = mktSalePrice,
            mktDtmAssetSymbol = mintingPolicyCurrencySymbol carbonTokenPolicy,
            mktDtmAssetName = tokenNameToPlutus tn,
            mktDtmAmount = qty
          }
  marketPlaceAddr <- scriptAddress $ marketplaceValidator mktParams script

  return $
    mustMint (GYMintScript carbonNftPolicy) unitRedeemer tn 1
      <> mustMint (GYMintScript carbonTokenPolicy) unitRedeemer tn qty
      <> mustHaveInput (GYTxIn oref GYTxInWitnessKey)
      <> mustHaveOutput (mkGYTxOut marketPlaceAddr (valueSingleton carbonToken qty) (datumFromPlutusData marketPlaceDatum))
      <> mustHaveOutput (mkGYTxOut ownerAddr (valueSingleton carbonNftToken 1) (datumFromPlutusData ()))
