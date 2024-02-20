module EA.Tx.Changeblock.Marketplace (MarketplaceInfo (..)) where

import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool, eaGetCollateral, eaLiftEither)
import GeniusYield.Types 
import GeniusYield.TxBuilder

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

data MarketplaceDatum = MarketplaceDatum
  { mktInfoOwner :: GYPubKeyHash
  , mktInfoSalePrice :: Integer
  , mktInfoCarbonPolicyId :: GYMintingPolicyId
  , mktInfoCarbonAssetName :: GYTokenName
  , mktInfoAmount :: Integer
  , mktInfoIssuer :: GYPubKeyHash
  , mktInfoIsSell :: Integer
  }
  deriving stock (Show)

marketplaceAtTxOutref :: GYTxOutRef -> EAApp( Either String MarketplaceInfo)
marketplaceAtTxOutref oref = do
    providers <- asks eaAppEnvGYProviders
    [utxo] <- liftIO $ gyQueryUtxosAtTxOutRefsWithDatums providers [oref]
    let result = utxoDatumPure utxo

    return $ case result of
        Left msg -> Left "Datum not found"
        Right datum -> 
                let (addr, val, MarketplaceDatum owner salePrice  assetCS  assetTN   assetAmount issuer   isSell) = datum
                    info = MarketplaceInfo {
                        mktInfoTxOutRef = oref
                        , mktInfoValue = val
                        , mktInfoOwner = owner
                        , mktInfoSalePrice = salePrice
                        , mktInfoCarbonPolicyId = assetCS
                        , mktInfoCarbonAssetName = assetTN
                        , mktInfoAmount = assetAmount
                        , mktInfoIssuer = issuer   
                        , mktInfoIsSell = isSell
                        }
                in Right info