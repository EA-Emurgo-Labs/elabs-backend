module EA.Tx.Changeblock.Marketplace () where

import GeniusYield.Types (
    GYTxoutRef,
    GYValue,
    GYPubKeyHash,
    GYMintingPolicyId,
    GYTokenName,
    )

data MarketplaceInfo = MarketplaceInfo {
   mktInfOref :: GYTxoutRef,
   mktInfValue :: GYValue,
   mktInfOwner  :: GYPubKeyHash,
   mktInfSalePrice  :: Integer,
   mktInfCarbonPolicyId :: GYMintingPolicyId,
   mktInfCarbonTokenName :: GYTokenName,
   mktInfCarbonTokenAmount :: Integer,
   mktInfIssuer :: GYPubKeyHash,
   mktInfIsSell :: Integer
 }

marketplaceAtTxOutref :: GYTxoutRef ->  Either String MarketplaceInfo