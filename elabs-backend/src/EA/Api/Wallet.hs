module EA.Api.Wallet (
  WalletApi,
  handleWalletApi,
) where

import Servant ( Capture, type (:>), JSON, Get )

import EA (EAApp)
import EA.Api.Types (UserId, WalletResponse (WalletResponse))
import EA.Wallet (eaGetUnusedAddresses, eaCreateAddresses)

--------------------------------------------------------------------------------

type WalletApi =
  "wallet"
    :> Capture "userId" UserId
    :> Get '[JSON] WalletResponse

handleWalletApi :: UserId -> EAApp WalletResponse
handleWalletApi userid = do
  addresses <- eaGetUnusedAddresses userid
  WalletResponse <$>
    if null addresses then
      eaCreateAddresses userid
    else
      return addresses
