module EA.Tx.OneShotMint (oneShotMint) where

import GeniusYield.TxBuilder (
  GYTxSkeleton,
  mustHaveInput,
  mustHaveOutput,
  mustMint,
 )
import GeniusYield.Types (
  GYAddress,
  GYAssetClass (GYToken),
  GYMintScript (GYMintScript),
  GYMintingPolicy,
  GYTxIn (GYTxIn),
  GYTxInWitness (GYTxInWitnessKey),
  GYTxOut (GYTxOut),
  GYTxOutRef,
  PlutusVersion (PlutusV2),
  mintingPolicyId,
  unitRedeemer,
  unsafeTokenNameFromHex,
  valueSingleton,
 )

oneShotMint ::
  GYAddress ->
  GYTxOutRef ->
  Integer ->
  GYMintingPolicy 'PlutusV2 ->
  GYTxSkeleton 'PlutusV2
oneShotMint out oref n policy =
  let
    ac = GYToken (mintingPolicyId policy) (unsafeTokenNameFromHex "")
    value = valueSingleton ac n
   in
    mustMint (GYMintScript policy) unitRedeemer (unsafeTokenNameFromHex "") n
      <> mustHaveInput (GYTxIn oref GYTxInWitnessKey)
      <> mustHaveOutput (GYTxOut out value Nothing Nothing)