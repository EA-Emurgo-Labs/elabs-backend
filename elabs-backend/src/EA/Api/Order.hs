module EA.Api.Order (
  OrderApi,
  handleOrderApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA (EAApp, EAAppEnv (..), eaLiftEither, eaLiftMaybe, eaMarketplaceAtTxOutRef, eaMarketplaceInfos, eaSubmitTx)
import EA.Api.Types (SubmitTxResponse, UserId (UserId), txBodySubmitTxResponse)
import Servant (
  GenericMode ((:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
  QueryParam,
  ReqBody,
  ToServantApi,
  type (:>),
 )
import Servant.Swagger (HasSwagger (toSwagger))

import EA.Script (Scripts, oracleValidator)
import EA.Script.Marketplace (MarketplaceInfo (MarketplaceInfo, mktInfoAmount, mktInfoIsSell, mktInfoOwner), MarketplaceParams (..), MarketplaceSellInfo (M_SELL))
import EA.Script.Marketplace qualified as Marketplace
import EA.Script.Oracle (OracleInfo)
import EA.Tx.Changeblock.Marketplace (adjustOrders, buy, cancel, partialBuy, sell)
import EA.Wallet (eaGetAddresses, eaGetCollateralFromInternalWallet)
import GeniusYield.TxBuilder (GYTxSkeleton, runGYTxMonadNode)
import GeniusYield.Types
import Internal.Wallet qualified as Wallet

--------------------------------------------------------------------------------

data OrderApi mode = OrderApi
  { listOrders :: mode :- OrderList
  , orderCreate :: mode :- OrderCreate
  , orderBuy :: mode :- OrderBuy
  , orderCancel :: mode :- OrderCancel
  , orderUpdate :: mode :- OrderUpdate
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes OrderApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi OrderApi))

handleOrderApi :: ServerT (NamedRoutes OrderApi) EAApp
handleOrderApi =
  OrderApi
    { listOrders = handleListOrders
    , orderCreate = handleOrderRequestSell
    , orderBuy = handleOrderBuy
    , orderCancel = handleOrderCancel
    , orderUpdate = handleOrderUpdate
    }

type OrderList =
  "orders"
    :> QueryParam "ownerUserId" Natural
    :> QueryParam "orderType" Int
    :> Get '[JSON] [MarketplaceInfo]

type OrderCreate =
  "orders"
    :> ReqBody '[JSON] OrderSellRequest
    :> "create"
    :> Post '[JSON] SubmitTxResponse

type OrderBuy =
  "orders"
    :> ReqBody '[JSON] OrderBuyRequest
    :> "buy"
    :> Post '[JSON] SubmitTxResponse

type OrderCancel =
  "orders"
    :> ReqBody '[JSON] OrderCancelRequest
    :> "cancel"
    :> Post '[JSON] SubmitTxResponse

type OrderUpdate =
  "orders"
    :> ReqBody '[JSON] OrderUpdateRequest
    :> "update-sale-price"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data OrderSellRequest = OrderSellRequest
  { ownerUserId :: !UserId
  -- ^ The user ID. The owner of the order.
  , sellReqAmount :: !Natural
  -- ^ The amount of carbon to mint.
  , sellReqPrice :: !Natural
  -- ^ The sell price per unit of carbon.
  , sellReqOrderUtxoRef :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)
data OrderUpdateRequest = OrderUpdateRequest
  { ownerUserId :: !UserId
  -- ^ The user ID. The owner of the order.
  , updatedPrice :: !Natural
  -- ^ The sell price per unit of carbon.
  , orderUtxoRef :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderCancelRequest = OrderCancelRequest
  { ownerUserId :: !UserId
  -- ^ The user ID who is owner of the order.
  , cancelOrderUtxo :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderBuyRequest = OrderBuyRequest
  { buyerUserId :: !UserId
  -- ^ The user ID.
  , buyAmount :: !Natural
  -- ^ The amount of carbon to buy.
  , orderUtxo :: !GYTxOutRef
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data MarketplaceApiCtx = MarketplaceApiCtx
  { mktCtxNetworkId :: !GYNetworkId
  , mktCtxProviders :: !GYProviders
  , mktCtxOracleRefInput :: !OracleInfo
  , mktCtxMarketplaceRefScript :: !(Maybe GYTxOutRef)
  , mktCtxScripts :: !Scripts
  , mktCtxCollateral :: (Maybe (GYTxOutRef, Bool), Wallet.PaymentKey)
  , mktCtxParams :: !MarketplaceParams
  }

withMarketplaceApiCtx :: (MarketplaceApiCtx -> EAApp a) -> EAApp a
withMarketplaceApiCtx f = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders

  oracleInfo <-
    asks eaAppEnvOracleRefInputUtxo
      >>= eaLiftMaybe "No Oracle found"

  oracleNftPolicyId <-
    asks eaAppEnvOracleNftMintingPolicyId
      >>= eaLiftMaybe "No Oracle PolicyId"

  oracleNftTknName <-
    asks eaAppEnvOracleNftTokenName
      >>= eaLiftMaybe "No Oracle TokenName"

  marketplaceRefScript <- asks eaAppEnvMarketplaceRefScriptUtxo
  oracleOperatorPubKeyHash <- asks eaAppEnvOracleOperatorPubKeyHash
  escrowPubkeyHash <- asks eaAppEnvMarketplaceEscrowPubKeyHash
  version <- asks eaAppEnvMarketplaceVersion
  scripts <- asks eaAppEnvScripts

  let oracleNftAssetClass = GYToken oracleNftPolicyId oracleNftTknName
      oracleValidatorHash = validatorHash $ oracleValidator oracleNftAssetClass oracleOperatorPubKeyHash scripts
      marketplaceParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = version
          , mktPrmOracleSymbol = oracleNftPolicyId
          , mktPrmOracleTokenName = oracleNftTknName
          }
  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  f $
    MarketplaceApiCtx
      { mktCtxNetworkId = nid
      , mktCtxProviders = providers
      , mktCtxOracleRefInput = oracleInfo
      , mktCtxMarketplaceRefScript = marketplaceRefScript
      , mktCtxScripts = scripts
      , mktCtxCollateral = (collateral, colKey)
      , mktCtxParams = marketplaceParams
      }

handleTx :: MarketplaceApiCtx -> GYAddress -> Wallet.PaymentKey -> GYTxSkeleton 'PlutusV2 -> EAApp SubmitTxResponse
handleTx MarketplaceApiCtx {..} addr addrKey tx = do
  txBody <- liftIO $ runGYTxMonadNode mktCtxNetworkId mktCtxProviders [addr] addr (fst mktCtxCollateral) (return tx)
  void $ eaSubmitTx $ Wallet.signTx txBody [snd mktCtxCollateral, addrKey]
  return $ txBodySubmitTxResponse txBody

handleOrderRequestSell :: OrderSellRequest -> EAApp SubmitTxResponse
handleOrderRequestSell OrderSellRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef sellReqOrderUtxoRef

  -- validate if user can request to sell order
  void $ eaLiftEither show $ validateRequestSale marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybe ("No addresses found with Owner:  " <> show (mktInfoOwner marketplaceInfo))
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddresses ownerUserId

  handleTx mCtx ownerAddr ownerKey $
    adjustOrders mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript (toInteger sellReqPrice) (toInteger sellReqAmount) Marketplace.M_SELL mktCtxParams mktCtxScripts
  where
    -- TODO: Better error handling
    validateRequestSale :: MarketplaceInfo -> Either String ()
    validateRequestSale MarketplaceInfo {..} = do
      when (mktInfoIsSell == M_SELL && sellReqAmount <= fromInteger mktInfoAmount) $ Left "Order is already on sell"
      when (sellReqAmount > fromInteger mktInfoAmount) $ Left "Cannot request sell more than amount in order"

-- API to handle buying an order
handleOrderBuy :: OrderBuyRequest -> EAApp SubmitTxResponse
handleOrderBuy OrderBuyRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef orderUtxo

  -- validate if buyer can buy order
  void $ eaLiftEither show $ validateRequest marketplaceInfo

  -- Get the user address & signing key  from user ID
  (buyerAddr, buyerKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses buyerUserId

  buyerPubkeyHash <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash buyerAddr)
  let tx =
        if isPartial marketplaceInfo
          then -- Partial buy
            partialBuy mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput buyerPubkeyHash (toInteger buyAmount) mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts
          else -- Full buy
            buy mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput buyerPubkeyHash mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts

  handleTx mCtx buyerAddr buyerKey tx
  where
    -- TODO: Better error handling
    validateRequest :: MarketplaceInfo -> Either String ()
    validateRequest MarketplaceInfo {..} = do
      when (mktInfoIsSell /= M_SELL) $ Left "Order is not For Sale"
      when (buyAmount > fromInteger mktInfoAmount) $ Left "Cannot buy more than amount in order"

    isPartial :: MarketplaceInfo -> Bool
    isPartial MarketplaceInfo {..} = buyAmount < fromInteger mktInfoAmount

handleOrderCancel :: OrderCancelRequest -> EAApp SubmitTxResponse
handleOrderCancel OrderCancelRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef cancelOrderUtxo

  -- validate if user can cancel order
  void $ eaLiftEither show $ validateCancelOrder marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybe ("No addresses found with Owner:  " <> show (mktInfoOwner marketplaceInfo))
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddresses ownerUserId

  handleTx mCtx ownerAddr ownerKey $ cancel mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts
  where
    validateCancelOrder :: MarketplaceInfo -> Either String ()
    validateCancelOrder MarketplaceInfo {..} = do
      when (mktInfoIsSell /= M_SELL) $ Left "Order is not For Sale"

handleOrderUpdate :: OrderUpdateRequest -> EAApp SubmitTxResponse
handleOrderUpdate OrderUpdateRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef orderUtxoRef

  -- validate if user can cancel order
  void $ eaLiftEither show $ validateUpdateOrder marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybe ("No addresses found with Owner:  " <> show (mktInfoOwner marketplaceInfo))
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddresses ownerUserId

  handleTx mCtx ownerAddr ownerKey $ sell mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript (toInteger updatedPrice) mktCtxParams mktCtxScripts
  where
    validateUpdateOrder :: MarketplaceInfo -> Either String ()
    validateUpdateOrder MarketplaceInfo {..} = do
      when (updatedPrice <= 0) $ Left "Price must be greater than 0"
      when (updatedPrice == fromInteger mktInfoAmount) $ Left "Price must be different from current price"
      when (mktInfoIsSell /= M_SELL) $ Left "Can only update price for sell orders"

handleListOrders :: Maybe Natural -> Maybe Int -> EAApp [MarketplaceInfo]
handleListOrders ownerUserId orderType = withMarketplaceApiCtx $ \MarketplaceApiCtx {..} -> do
  mInfos <- asks eaMarketplaceInfos mktCtxParams
  ownerPubkeys <- mOwnerPubKeyHashes
  return $ filter (\m -> filterByOwner ownerPubkeys m && filterByType m) mInfos
  where
    mOwnerPubKeyHashes :: EAApp [GYPubKeyHash]
    mOwnerPubKeyHashes = do
      case ownerUserId of
        Nothing -> return []
        Just uid -> do
          addrs <- eaGetAddresses $ UserId uid
          return $ mapMaybe (addressToPubKeyHash . fst) addrs

    filterByOwner :: [GYPubKeyHash] -> MarketplaceInfo -> Bool
    filterByOwner pubkeyHashes MarketplaceInfo {..} = isNothing ownerUserId || mktInfoOwner `elem` pubkeyHashes

    filterByType :: MarketplaceInfo -> Bool
    filterByType MarketplaceInfo {..} = maybe True (\ot -> mktInfoIsSell == toEnum ot) orderType