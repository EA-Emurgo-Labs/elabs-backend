module EA.Tx.Enum.EVal (updateEVal, spendEval) where

import EA.Script (Scripts (..), eMintingPolicy)
import EA.Script.Enum (EDatum (..), EMintInfo (..))
import EA.Script.Enum qualified as Enum
import GeniusYield.TxBuilder (GYTxSkeleton, mustBeSignedBy, mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types

type EAddress = GYAddress

type MinterPkh = GYPubKeyHash

mkTokenValue :: ESpendInfo -> Integer -> GYValue 
mkTokenValue ESpendInfo {..} = valueSingleton (GYToken eInfoPolicyId eInfoTokenName)

updateEval :: 
  GYNetworkId ->
  EMintInfo -> 
  Scripts ->
  GYTxOutRef -> 
  GYTxSkeleton 'PlutusV2
updateEval nid info@ESpendInfo {..} scripts oref =
  let policy = eMintingPolicy scripts 
      action = redeemerFromPlutusData EAction.Beta
      tn = eInfoTokenName
      inDatum = 
        EDatum { a = eInfoDatum }
      outDatum =
        EDatum { a = eInfoDatum + 1 }
      input = GYTxIn 
              { gyTxInTxOutRef = oref 
              , gyTxInWitness = GYTxInWitnessScript 
                  (GYInScript (validatorToScript $ eSpendingValidator params scripts))
                  (datumFromPlutusData inDatum)
                  (action)
              }
      output = mkGYTxOut eInfoAddress (mkTokenValue info eInfoAmt) (datumFromPlutus outDatum)

  in mustHaveInput input
    <> mustHaveOutput output 

spendEval :: 
  GYNetworkId ->
  EMintInfo -> 
  Scripts ->
  GYTxOutRef -> 
  GYTxSkeleton 'PlutusV2
spendEval nid info@ESpendInfo {..} scripts oref = 
  let policy = eMintingPolicy scripts 
      action = redeemerFromPlutusData EAction.Beta
      tn = eInfoTokenName
      inDatum = 
        EDatum { a = eInfoDatum }
      input = GYTxIn 
              { gyTxInTxOutRef = oref 
              , gyTxInWitness = GYTxInWitnessScript 
                  (GYInScript (validatorToScript $ eSpendingValidator params scripts))
                  (datumFromPlutusData inDatum)
                  (action)
              }

  in mustMint policy action tn (-1)
    <> mustHaveInput input

