module Internal.Wallet (
  RootKey,
  PaymentKey,
  deriveAddress,
  readRootKey,
  writeRootKey,
  genRootKeyFromMnemonic,
) where

import Data.Tagged (Tagged)

import GeniusYield.Types (
  GYAddress,
  GYNetworkId (GYMainnet),
  addressFromTextMaybe,
 )
import GeniusYield.Types.Key.Class (
  ToShelleyWitnessSigningKey (toShelleyWitnessSigningKey),
 )

import Cardano.Address (
  HasNetworkDiscriminant (NetworkDiscriminant),
  bech32,
 )
import Cardano.Address.Derivation (
  Depth (AccountK, PaymentK, RootK),
  DerivationType (Hardened, Soft),
  GenMasterKey (genMasterKeyFromMnemonic),
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

import Cardano.Api.Shelley (
  ShelleyWitnessSigningKey (WitnessPaymentExtendedKey),
  SigningKey (PaymentExtendedSigningKey),
 )
import Cardano.Mnemonic (SomeMnemonic)
import Data.ByteString qualified as BS

--------------------------------------------------------------------------------

deriveAddress ::
  GYNetworkId ->
  RootKey ->
  Tagged "Account" Int64 ->
  Tagged "Address" Int64 ->
  Either String (GYAddress, PaymentKey)
deriveAddress nid (RootKey rootK) acc addr = do
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

  maybe
    (Left "Cannot decode beck to GYAddress")
    Right
    ( do
        gyAddr <- addressFromTextMaybe . bech32 $ addr
        return (gyAddr, PaymentKey addrK)
    )

network :: GYNetworkId -> NetworkDiscriminant Shelley
network GYMainnet = S.shelleyMainnet
network _ = S.shelleyTestnet

--------------------------------------------------------------------------------
-- Payment key
--
-- Dont expose the internals of the payment key
-- Dont implement any instances other than ToShelleyWitnessSigningKey

newtype PaymentKey = PaymentKey {_unPaymentKey :: Shelley 'PaymentK XPrv}

instance ToShelleyWitnessSigningKey PaymentKey where
  toShelleyWitnessSigningKey (PaymentKey key) =
    WitnessPaymentExtendedKey (PaymentExtendedSigningKey (getKey key))

--------------------------------------------------------------------------------
-- Root key
--
-- Dont expose the internals of the root key
-- Dont implement any instances

newtype RootKey = RootKey {_unRootKey :: Shelley 'RootK XPrv}

readRootKey :: FilePath -> IO (Maybe RootKey)
readRootKey fp = do
  bs <- BS.readFile fp
  return $ do
    key <- liftXPrv <$> xprvFromBytes bs
    return $ RootKey key

writeRootKey :: FilePath -> RootKey -> IO ()
writeRootKey fp (RootKey key) = BS.writeFile fp (xprvToBytes . getKey $ key)

genRootKeyFromMnemonic :: SomeMnemonic -> RootKey
genRootKeyFromMnemonic mw =
  RootKey $ genMasterKeyFromMnemonic @Shelley mw mempty
