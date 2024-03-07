module EA.Api.Carbon (
  CarbonApi,
  handleCarbonApi,
)
where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Encoding.Base16 (encodeBase16)
import EA (
  EAApp,
  EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvGYProviders, eaAppEnvMarketplaceEscrowPubKeyHash, eaAppEnvMarketplaceVersion, eaAppEnvOracleNftMintingPolicyId, eaAppEnvOracleNftTokenName, eaAppEnvOracleOperatorPubKeyHash, eaAppEnvScripts),
  eaLiftEither,
  eaLiftMaybe,
  eaSubmitTx,
 )
import EA.Api.Types (
  CarbonMintRequest (amount, sell, userId),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import EA.Orphans (MultipartFormDataTmp)
import EA.Script (marketplaceValidator, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
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
import Servant (JSON, Post, Tagged, type (:>))
import Servant.Multipart (
  MultipartData,
  Tmp,
  lookupFile,
  lookupInput,
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> MultipartFormDataTmp
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------
-- Swagger woraround instance for 'CarbonApi'

instance {-# OVERLAPPING #-} HasSwagger CarbonApi where
  toSwagger _ = toSwagger (Proxy :: Proxy CarbonMint')

type CarbonMint' =
  "carbon"
    :> Tagged CarbonMintRequest MultipartFormDataTmp
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------

data CarbonMintResponse = CarbonMintResponse
  { ipfsHash :: !Text
  , ipfsName :: !Text
  , ipfsSize :: !Text
  , ipfsPinningState :: !Text
  , submitTxInfo :: !SubmitTxResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleCarbonApi ::
  MultipartData Tmp ->
  EAApp CarbonMintResponse
handleCarbonApi multipartData = do
  filePart <- eaLiftEither id $ lookupFile "file" multipartData
  dataPart <- eaLiftEither id $ lookupInput "data" multipartData

  request <-
    eaLiftMaybe "Cannot decode JSON data" $
      Aeson.decode @CarbonMintRequest $
        encodeUtf8 dataPart

  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion
  escrowPubkeyHash <- asks eaAppEnvMarketplaceEscrowPubKeyHash
  operatorPubkeyHash <- asks eaAppEnvOracleOperatorPubKeyHash
  oracleAssetName <- asks eaAppEnvOracleNftTokenName >>= eaLiftMaybe "No Oracle NFT token name"
  oraclePolicyId <- asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybe "No Oracle NFT minting policy ID"

  -- Get the internal address pairs.
  internalAddrPairs <- eaGetInternalAddresses False

  -- Get the user address. We don't need the signing key here.
  (userAddr, _) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (userId request)

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, oref) <-
    eaSelectOref
      internalAddrPairs
      (\r -> collateral /= Just (r, True))
      >>= eaLiftMaybe "No UTxO found"

  -- TODO: Issuer should be Internal Wallet address ?
  issuer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash userAddr)
  owner <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash userAddr)

  let orcValidatorHash =
        validatorHash $ oracleValidator (GYToken oraclePolicyId oracleAssetName) operatorPubkeyHash scripts

      marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = marketplaceVersion
          , mktPrmOracleSymbol = oraclePolicyId
          , mktPrmOracleTokenName = oracleAssetName
          }

  marketplaceAddress <-
    liftIO $
      runGYTxQueryMonadNode nid providers $
        scriptAddress (marketplaceValidator marketParams scripts)

  ipfsAddResp <- ipfsAddFile filePart
  ipfsPinObjResp <- ipfsPinObject ipfsAddResp.ipfs_hash

  let
    tokenName =
      unsafeTokenNameFromHex $
        encodeBase16 $
          T.take 10 $
            T.append "CBLK" ipfsAddResp.ipfs_hash

    tx =
      mintIpfsNftCarbonToken
        oref
        marketplaceAddress
        userAddr
        owner
        issuer
        tokenName
        (toInteger $ sell request)
        (toInteger $ amount request)
        scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

  return $
    CarbonMintResponse
      ipfsAddResp.ipfs_hash
      ipfsAddResp.name
      ipfsAddResp.size
      ipfsPinObjResp.state
      (txBodySubmitTxResponse txBody)
