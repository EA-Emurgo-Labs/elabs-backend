module EA.Tx.Enum.EVal (updateEVal, spendEval) where

import EA.Script (Scripts (..), eMintingPolicy)
import EA.Script.Enum (EDatum (..), EMintInfo (..))
import EA.Script.Enum qualified as Enum
import GeniusYield.TxBuilder (GYTxSkeleton, mustBeSignedBy, mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types

type EAddress = GYAddress

type MinterPkh = GYPubKeyHash

mkTokenValue :: EMintInfo -> Integer -> GYValue 
mkTokenValue EMintInfo {..} = valueSingleton (GYToken eInfoPolicyId eInfoTokenName)

updateEval :: 

spendEval :: 

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

