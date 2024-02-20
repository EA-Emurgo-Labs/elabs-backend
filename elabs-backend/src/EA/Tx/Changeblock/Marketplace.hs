module EA.Tx.Changeblock.Marketplace (MarketplaceInfo (..)) where

import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaGetCollateral, eaLiftEither)
import GeniusYield.Types (GYMintingPolicyId, GYPubKeyHash, GYTokenName, GYTxOutRef, GYValue, gyQueryUtxosAtTxOutRefsWithDatums)

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

marketplaceAtTxOutref :: GYTxOutRef ->  Either String MarketplaceInfo
marketplaceAtTxOutref oref = do
    providers <- asks eaAppEnvGYProviders
    [(utxo, datum)] <- liftIO $ gyQueryUtxosAtTxOutRefsWithDatums providers [oref]
    case datum of
        Nothing -> Left "Datum not found"
        Just ( owner         
    , salePrice  
    , assetCS    
    , assetTN    
    , assetAmount 
    , issuer     
    , isSell ) -> Right (MarketplaceInfo {
        mktInfoTxOutRef = oref
  , mktInfoValue = utxoValue utxo
  , mktInfoOwner = owner
  , mktInfoSalePrice = salePrice
  , mktInfoCarbonPolicyId = assetCS
  , mktInfoCarbonAssetName = assetTN
  , mktInfoAmount = assetAmount
  , mktInfoIssuer = issuer   
  , mktInfoIsSell = isSell
    })


