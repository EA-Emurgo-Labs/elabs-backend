module EA.Api.Carbon
  ( CarbonApi,
    CarbonMintRequest (..),
    handleCarbonMint,
  )
where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Encoding.Base16 (encodeBase16)
import EA (EAApp, EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvGYProviders, eaAppEnvScripts), eaLiftEither, eaLiftMaybe, eaLogInfo, eaSubmitTx)
import EA.Api.Types (SubmitTxResponse, UserId, txBodySubmitTxResponse)
import EA.Script (nftMintingPolicy, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.Operation (mintIpfsNftCarbonToken)
import EA.Wallet (eaGetAddresses, eaGetCollateralFromInternalWallet, eaGetInternalAddresses, eaSelectOref)
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (GYAssetClass (GYToken), mintingPolicyId, unsafeTokenNameFromHex, validatorHash)
import GeniusYield.Types.Address (addressToPubKeyHash)
import Internal.Ipfs (ipfsAddFile, ipfsPinObject)
import Internal.Ipfs.Types (IpfsAddResponse (..), IpfsPin (..))
import Internal.Wallet qualified as Wallet
import Servant (Header, JSON, Post, type (:>))
import Servant.Multipart
  ( MultipartData,
    MultipartForm,
    Tmp,
    lookupFile,
    lookupInput,
  )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> MultipartForm Tmp (MultipartData Tmp)
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------
-- FIXME: This is because of MultiparForm, which is not supported by HasSwagger

type CarbonMintFix =
  "carbon"
    :> Header "user_id" UserId
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

instance {-# OVERLAPPING #-} HasSwagger CarbonApi where
  toSwagger _ = toSwagger (Proxy :: Proxy CarbonMintFix)

--------------------------------------------------------------------------------

data CarbonMintRequest = CarbonMintRequest
  { -- | The user ID.
    userId :: !UserId,
    -- | The amount of carbon to mint.
    amount :: !Natural,
    -- | The sell price per unit of carbon.
    sell :: !Natural
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data CarbonMintResponse = CarbonMintResponse
  { ipfsHash :: !Text,
    ipfsName :: !Text,
    ipfsSize :: !Text,
    ipfsPinningState :: !Text,
    submitTxInfo :: !SubmitTxResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleCarbonMint ::
  MultipartData Tmp ->
  EAApp CarbonMintResponse
handleCarbonMint multipartData = do
  filePart <- eaLiftEither id $ lookupFile "file" multipartData
  dataPart <- eaLiftEither id $ lookupInput "data" multipartData

  request <-
    eaLiftMaybe "Cannot decode JSON data" $
      Aeson.decode @CarbonMintRequest $
        encodeUtf8 dataPart

  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts
  internalAddrPairs <- eaGetInternalAddresses
  pairs <- eaGetAddresses (userId request)
  (userAddr, _) <- eaLiftMaybe "No addresses found" (listToMaybe pairs)
  (collateral, colKey) <- eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"
  (addr, key, oref) <- eaSelectOref providers (pairs ++ internalAddrPairs) (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

  issuer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash addr)

  -- TODO: User proper policyId for Oracle NFT
  let oracleNftAsset = mintingPolicyId $ nftMintingPolicy oref scripts
      oracleNftAssetName = unsafeTokenNameFromHex "43424c"
      orcAssetClass = GYToken oracleNftAsset oracleNftAssetName
      -- TODO: user proper operaor pubkey hash for oracle validator
      orcValidatorHash = validatorHash $ oracleValidator orcAssetClass issuer scripts
      marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash,
            mktPrmEscrowValidator = issuer, -- TODO: User proper pubkeyhash of escrow
            mktPrmVersion = unsafeTokenNameFromHex "76312e302e30", -- It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftAsset,
            mktPrmOracleTokenName = oracleNftAssetName
          }

  ipfsAddResp <- ipfsAddFile filePart
  ipfsPinObjResp <- ipfsPinObject ipfsAddResp.ipfs_hash

  eaLogInfo "carbon-mint" $ show request
  eaLogInfo "carbon-mint" $ "IPFS HASH" <> show ipfsAddResp.ipfs_hash
  let tokenName = unsafeTokenNameFromHex $ encodeBase16 $ T.take 10 $ T.append "CBLK" ipfsAddResp.ipfs_hash
  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral $
        mintIpfsNftCarbonToken oref marketParams userAddr issuer tokenName (toInteger $ sell request) (toInteger $ amount request) scripts

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

  return $
    CarbonMintResponse
      ipfsAddResp.ipfs_hash
      ipfsAddResp.name
      ipfsAddResp.size
      ipfsPinObjResp.state
      (txBodySubmitTxResponse txBody)
