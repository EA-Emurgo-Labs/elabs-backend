module EA.Api.Wallet (
  WalletApi,
  handleWalletApi,
  handleWalletBalanceApi,
) where

import Servant (Capture, GenericMode ((:-)), Get, HasServer (ServerT), JSON, NamedRoutes, ToServantApi, type (:>))

import EA (EAApp, eaGetAddressValue)
import EA.Api.Types (UserId, WalletResponse (WalletResponse), WalletValueResp (WalletValueResp), walletAddressWithPubKeyHash)
import EA.Wallet (eaGetAddresses)
import GeniusYield.Types (GYValue, valueSplitAda)
import Internal.AdaPrice (getAdaPrice)
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------
data WalletApi mode = WalletApi
  { getWalletAddress :: mode :- WalletResp
  , getWalletBalance :: mode :- WalletBalance
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes WalletApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi WalletApi))

handleWalletApi :: ServerT (NamedRoutes WalletApi) EAApp
handleWalletApi =
  WalletApi
    { getWalletAddress = handleWalletAddressApi
    , getWalletBalance = handleWalletBalanceApi
    }

type WalletResp =
  "wallet"
    :> Capture "user" UserId
    :> Get '[JSON] WalletResponse

type WalletBalance =
  "wallet"
    :> Capture "user" UserId
    :> "balance"
    :> Get '[JSON] WalletValueResp

handleWalletAddressApi :: UserId -> EAApp WalletResponse
handleWalletAddressApi userid = do
  addrs <- eaGetAddresses userid
  return $ WalletResponse (map (walletAddressWithPubKeyHash . fst) addrs) userid

handleWalletBalanceApi :: UserId -> EAApp WalletValueResp
handleWalletBalanceApi userid = do
  addrs <- eaGetAddresses userid
  value <- eaGetAddressValue (map fst addrs)
  adaPrice <- liftIO getAdaPrice
  let totalAdaValueUsd = calcTotAdaPrice value =<< adaPrice

  return $ WalletValueResp value adaPrice totalAdaValueUsd
  where
    calcTotAdaPrice :: GYValue -> Double -> Maybe Double
    calcTotAdaPrice value adaPrice =
      let adaAmt = fst $ valueSplitAda value
       in Just $ (fromIntegral adaAmt / 1000000) * adaPrice
