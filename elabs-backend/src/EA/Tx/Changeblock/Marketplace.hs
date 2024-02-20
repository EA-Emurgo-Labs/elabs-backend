module EA.Tx.Changeblock.Marketplace (MarketplaceInfo (..)) where

import GeniusYield.Types (GYMintingPolicyId, GYPubKeyHash, GYTokenName, GYTxOutRef, GYValue)

data MarketplaceInfo = MarketplaceInfo
  { mktInfoTxOutRef :: GYTxOutRef
  , mktInfoValue :: GYValue
  , mktInfoOwner :: GYPubKeyHash
  , mktInfoSalePrice :: Integer
  , mktInfoCarbonPolicyId :: GYMintingPolicyId
  , mktInfoCarbonAssetName :: GYTokenName
  , mktInfoAmount :: Integer
  , mktInfoIssuer :: GYPubKeyHash
  , mktInfoIsSell :: Integer
  }
  deriving stock (Show)