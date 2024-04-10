module EA.Api.Enum (

)
where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Encoding.Base16 (encodeBase16)
import EA (
  EAApp,
  EAAppEnv (
    eaAppEnvGYNetworkId,
    eaAppEnvGYProviders,
    eaAppEnvMarketplaceBackdoorPubKeyHash,
    eaAppEnvMarketplaceEscrowPubKeyHash,
    eaAppEnvMarketplaceVersion,
    eaAppEnvOracleNftMintingPolicyId,
    eaAppEnvOracleNftTokenName,
    eaAppEnvOracleOperatorPubKeyHash,
    eaAppEnvScripts
  ),
  eaLiftEitherServerError,
  eaLiftMaybe,
  eaLiftMaybeServerError,
  eaSubmitTx,
 )
import EA.Api.Types (
  CarbonMintRequest (amount, sell, userId),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import EA.Orphans (MultipartFormDataTmp)
import EA.EScript (
  Scripts (..),
  eMintingPolicy,
  eSpendingValidator
 )
import EA.Script.Enum (EParams (..))
import EA.Tx.Changeblock.MintIpfsNftCarbonToken (mintIpfsNftCarbonToken)
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
  eaSelectOref,
 )
import GeniusYield.TxBuilder (
  runGYTxMonadNode,
  runGYTxQueryMonadNode,
  scriptAddress,
 )
import GeniusYield.Types (
  GYAssetClass (GYToken),
  unsafeTokenNameFromHex,
  validatorHash,
 )
import GeniusYield.Types.Address (addressToPubKeyHash)
import Internal.Ipfs (ipfsAddFile, ipfsPinObject)
import Internal.Ipfs.Types (IpfsAddResponse (..), IpfsPin (..))
import Internal.Wallet qualified as Wallet
import Servant (JSON, Post, Tagged, err400, type (:>))
import Servant.Multipart (
  MultipartData,
  Tmp,
  lookupFile,
  lookupInput,
 )
import Servant.Swagger (HasSwagger (toSwagger))

---

data EnumApi mode = EnumApi 
  { enumMint :: mode :- EnumMint 
  , enumUpdate :: mode :- EnumUpdate 
  , enumBurn :: mode :- EnumBurn 
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes EnumApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi EnumApi))

handleEnumApi :: ServerT (NamedRoutes EnumApi) EAApp
handleEnumApi =
  EnumApi 
    { enumMint = handleEnumMint 
    , enumUpdate = handleEnumUpdate
    , enumBurn = handleEnumBurn 
    }

type EnumMint = 
  Description "This api mits the EnumToken and sends to the locking validator"
    :> "enum"
    :> ReqBody '[JSON] EnumMintRequest
    :> "mint"
    :> Post '[JSON] SubmitTxResponse 

type EnumUpdate =
  Description "This call will update the token datum"
    :> "enum"
    :> ReqBody '[JSON] EnumUpdateRequest
    :> "update"
    :> Post '[JSON] SubmitTxResponse
  
type EnumBurn = 
  Description "This api will burn the EnumToken"
    :> "enum" 
    :> ReqBody '[JSON] EnumBurnRequest
    :> "burn"
    :> Post '[JSON] SubmitTxResponse

---

data EnumApiCtx = EnumApiCtx
  { eCtxNetworkId :: !GYNetworkId
  , eCtxProviders :: !GYProviders 
  , eCtxScripts :: !Scripts
  , eCtxCollateral :: (Maybe (GYTxOutRef, Bool), Wallet.PaymentKey)
  , eCtxParams :: !Params
  }

-- applies the EnumApiCtx to the EAApp
withEnumApiCtx :: (EnumApiCtx -> EAApp a) -> EAApp a
withENumApiCtx f = do 
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders

  scripts <- asks eaAppEnvScripts

  -- let eMintPolicyId =
  --     eMintTokenName = 
  --     eValAddress = 
      
    
  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybeServerError err400 "No collateral found"

  f $ 
    EnumApiCtx
      { eCtxNetworkId = nid 
      , eCtxProviders = providers 
      , eCtxScripts = scripts 
      , eCtxCollateral = (collateral, colKey) 
      , eCtxParams = eParams
      }

handleTx :: EnumApiCtx -> GYAddress -> Wallet.PaymentKey -> GYTxSkeleton 'PlutusV2 -> EAApp SubmitTxResponse
handleTx EnumApiCtx {..} addr addrKey tx = do 
  txBody <- liftIO $ runGYTxMonadNode eCtxNetworkId eCtxProviders [addr] addr (fst eCtxCollateral) (return tx)
  void $ eaSubmitTx $ Wallet.signTx txBody [snd eCtxCollateral, addrKey]
  return $ txBodySubmitTxResponse txBody

handleEnumMint :: EnumMintRequest -> EAApp SubmitTxResponse
handleENumMint EnumMintRequest {..} = withEnumApiCtx $ \mCtx@EnumApiCtx {..} -> do 

  void $ eaLiftEitherServerError err400 show $ validateRequest eInfo

  (buyerAddr, buyerKey) <- 
    eaLiftMaybeServerError err400 "No Addresses Found" =<< eaGetAddressFromPubKeyhash buyer

  buyerPubkeyHash <- eaLiftEitherServerError err400 "Cannot Decode Address" (addressToPubKeyHash eMintBuyerAddr)

  handleTx mCtx userAddr userKey tx

  where 

    validateRequest :: EnumInfo -> Either String () 
    validateRequest EnumInfo {..} = do 
      when (eInfoAction /= E_Alpha) $ Left "Invalid Redeemer Action"

handleEnumUpdate :: EnumUpdateRequest -> EAApp SubmitTxResponse
handleEnumUpdate EnumUpdateRequest {..} = withEnumApiCtx $ \mCtx@EnumApiCtx {..} -> do 

  void $ eaLiftEitherServerError err400 show $ validateRequest eInfo

  (buyerAddr, buyerKey) <- 
    eaLiftMaybeServerError err400 "No Addresses Found" =<< eaGetAddressFromPubKeyhash buyer

  buyerPubkeyHash <- eaLiftEitherServerError err400 "Cannot Decode Address" (addressToPubKeyHash eMintBuyerAddr)

  handleTx mCtx userAddr userKey tx

  where 

    validateRequest :: EnumInfo -> Either String () 
    validateRequest EnumInfo {..} = do 
      when (eInfoAction /= E_Alpha) $ Left "Invalid Redeemer Action"

handleEnumBurn :: EnumBurnRequest -> EAApp SubmitTxResponse
handleEnumBurn EnumBurnRequest {..} = withEnumApiCtx $ \mCtx@EnumApiCtx {..} -> do 

  void $ eaLiftEitherServerError err400 show $ validateRequest eInfo

  (buyerAddr, buyerKey) <- 
    eaLiftMaybeServerError err400 "No Addresses Found" =<< eaGetAddressFromPubKeyhash buyer

  buyerPubkeyHash <- eaLiftEitherServerError err400 "Cannot Decode Address" (addressToPubKeyHash eMintBuyerAddr)

  handleTx mCtx userAddr userKey tx

  where 

    validateRequest :: EnumInfo -> Either String () 
    validateRequest EnumInfo {..} = do 
      when (eInfoAction /= E_Beta) $ Left "Invalid Redeemer Action"

