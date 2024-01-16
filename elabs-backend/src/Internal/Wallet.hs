module Internal.Wallet (
  deriveAddress,
) where

import Data.Aeson qualified as Aeson
import Data.Tagged (Tagged)

import GeniusYield.Types (GYAddress, GYNetworkId (GYMainnet))

import Cardano.Address (
  HasNetworkDiscriminant (NetworkDiscriminant),
  bech32,
 )
import Cardano.Address.Derivation (
  Depth (RootK),
  HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey),
  XPrv,
  indexFromWord32,
  toXPub,
 )
import Cardano.Address.Style.Shelley (Shelley)
import Cardano.Address.Style.Shelley qualified as S

--------------------------------------------------------------------------------

deriveAddress ::
  GYNetworkId ->
  Shelley 'RootK XPrv ->
  Tagged "Account" Int64 ->
  Tagged "Address" Int64 ->
  Maybe GYAddress
deriveAddress nid rootK acc addr = do
  -- TODO: secure Int64 to Word32
  accI <- indexFromWord32 . fromIntegral $ acc
  addrI <- indexFromWord32 . fromIntegral $ addr

  let
    acctK = deriveAccountPrivateKey rootK accI
    addrK = deriveAddressPrivateKey acctK S.UTxOExternal addrI
    stakeK = S.deriveDelegationPrivateKey acctK
    paymentCredential = S.PaymentFromExtendedKey (toXPub <$> addrK)
    delegationCredential = S.DelegationFromExtendedKey (toXPub <$> stakeK)
    addr =
      S.delegationAddress (network nid) paymentCredential delegationCredential

  -- TODO: Better Cardano.Address.Address to GYAddress convertion
  Aeson.decode @GYAddress . encodeUtf8 . bech32 $ addr

network :: GYNetworkId -> NetworkDiscriminant Shelley
network GYMainnet = S.shelleyMainnet
network _ = S.shelleyTestnet
