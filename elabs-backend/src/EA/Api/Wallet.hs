module EA.Api.Wallet (
  WalletApi,
  handleWalletApi,
) where

import Servant (Capture, Get, JSON, type (:>))

import EA (EAApp)
import EA.Api.Types (UserId, WalletResponse (WalletResponse), walletAddressWithPubKeyHash)
import EA.Wallet (eaGetAddresses)

--------------------------------------------------------------------------------

type WalletApi =
  "wallet"
    :> Capture "user" UserId
    :> Get '[JSON] WalletResponse

handleWalletApi :: UserId -> EAApp WalletResponse
handleWalletApi userid = do
  addrs <- eaGetAddresses userid
  return $ WalletResponse $ map (walletAddressWithPubKeyHash . fst) addrs
