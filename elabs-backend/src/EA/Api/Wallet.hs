module EA.Api.Wallet (
  WalletApi,
  handleWalletApi,
) where

import Servant (Capture, Get, JSON, type (:>))

import EA (EAApp)
import EA.Api.Types (UserId, WalletResponse (WalletResponse))
import EA.Wallet (eaGetUnusedAddresses)

--------------------------------------------------------------------------------

type WalletApi =
  "wallet"
    :> Capture "user" UserId
    :> Get '[JSON] WalletResponse

handleWalletApi :: UserId -> EAApp WalletResponse
handleWalletApi userid = do
  WalletResponse <$> eaGetUnusedAddresses userid
