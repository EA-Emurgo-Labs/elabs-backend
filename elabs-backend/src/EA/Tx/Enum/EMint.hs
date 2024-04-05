module EA.Tx.Enum.EMint (mintEMint, burnEMint) where

import EA.Script (Scripts (..), eMintingPolicy)
import EA.Script.Enum (EDatum (..), EMintInfo (..))
import EA.Script.Enum qualified as Enum
import GeniusYield.TxBuilder (GYTxSkeleton, mustBeSignedBy, mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types

type EAddress = GYAddress

type MinterPkh = GYPubKeyHash

mkTokenValue :: EMintInfo -> Integer -> GYValue 
mkTokenValue EMintInfo {..} = valueSingleton (GYToken eInfoPolicyId eInfoTokenName)

mintEMint :: 
  GYNetworkId ->
  EMintInfo -> 
  Scripts -> 
  GYTxSkeleton 'PlutusV2
mintEMint nid info@EMintInfo {..} scripts = 

  let policy = eMintingPolicy scripts 
      action = redeemerFromPlutusData EAction.Alpha
      tn = eInfoTokenName
      vAddr = eInfoAddress
      outValue = mkTokenValue info eInfoAmt
      outDatum = 
        EDatum { a = eInfoDatum }

  in mustMint policy action tn 1
    <> mustHaveOutput (mkGYTxOut vAddr outValue outDatum)

burnEMint :: 
  GYNetworkId ->
  GYTxOutRef -> 
  EMintInfo -> 
  EScriptParams ->
  Scripts -> 
  GYTxSkeleton 'PlutusV2
mintEMint nid oref info@EMintInfo {..} params scripts = 

  let policy = eMintingPolicy scripts 
      action = redeemerFromPlutusData EAction.Beta
      tn = eInfoTokenName
      outDatum = 
        EDatum { a = eInfoDatum }
      input = GYTxIn 
              { gyTxInTxOutRef = oref 
              , gyTxInWitness = GYTxInWitnessScript 
                  (GYInScript (validatorToScript $ eSpendingValidator params scripts))
                  (datumFromPlutusData outDatum)
                  (action)
              }

  in mustMint policy action tn (-1)
    <> mustHaveInput input