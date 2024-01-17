module Internal.Wallet (
  deriveAddress,
  toRootKey,
  fromRootKey,
) where

import Data.Tagged (Tagged)

import GeniusYield.Types (
  GYAddress,
  GYNetworkId (GYMainnet),
  addressFromTextMaybe,
 )

import Cardano.Address (
  HasNetworkDiscriminant (NetworkDiscriminant),
  bech32,
 )
import Cardano.Address.Derivation (
  Depth (AccountK, PaymentK, RootK),
  DerivationType (Hardened, Soft),
  HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey),
  Index (indexToWord32),
  XPrv,
  indexFromWord32,
  toXPub,
  xprvFromBytes,
  xprvToBytes,
 )
import Cardano.Address.Style.Shelley (Shelley, getKey, liftXPrv)
import Cardano.Address.Style.Shelley qualified as S

--------------------------------------------------------------------------------

deriveAddress ::
  GYNetworkId ->
  Shelley 'RootK XPrv ->
  Tagged "Account" Int64 ->
  Tagged "Address" Int64 ->
  Either String GYAddress
deriveAddress nid rootK acc addr = do
  -- indexFrom32 will return Nothing when the index is out of range
  accI <-
    maybe
      (Left "Cannot create account index")
      Right
      ( indexFromWord32
          ( fromIntegral acc
              + indexToWord32 (minBound @(Index 'Hardened 'AccountK))
          )
      )
  addrI <-
    maybe
      (Left "Cannot create address index")
      Right
      ( indexFromWord32
          ( fromIntegral addr
              + indexToWord32 (minBound @(Index 'Soft 'PaymentK))
          )
      )

  let
    acctK = deriveAccountPrivateKey rootK accI
    addrK = deriveAddressPrivateKey acctK S.UTxOExternal addrI
    stakeK = S.deriveDelegationPrivateKey acctK
    paymentCredential = S.PaymentFromExtendedKey (toXPub <$> addrK)
    delegationCredential = S.DelegationFromExtendedKey (toXPub <$> stakeK)
    addr =
      S.delegationAddress (network nid) paymentCredential delegationCredential

  -- TODO: Better Cardano.Address.Address to GYAddress convertion
  maybe
    (Left "Cannot decode beck to GYAddress")
    Right
    (addressFromTextMaybe . bech32 $ addr)

toRootKey :: ByteString -> Maybe (Shelley 'RootK XPrv)
toRootKey bs = liftXPrv <$> xprvFromBytes bs

fromRootKey :: Shelley 'RootK XPrv -> ByteString
fromRootKey = xprvToBytes . getKey

network :: GYNetworkId -> NetworkDiscriminant Shelley
network GYMainnet = S.shelleyMainnet
network _ = S.shelleyTestnet
