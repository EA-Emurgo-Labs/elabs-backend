module EA.Utils.Wallet (AccountKeys (..), deriveChildKeys, mkAddress) where

import Cardano.Address (bech32)
import Cardano.Address.Derivation
import Cardano.Address.Style.Shelley qualified as S
import Data.Text qualified as T
import GeniusYield.Types.NetworkId

data AccountKeys = AccountKeys
  { acctPaymentPrivKey :: !(S.Shelley 'PaymentK XPrv),
    acctStakePrivKey :: S.Shelley 'DelegationK XPrv
  }

-- TODO: Better error msg
deriveChildKeys ::
  S.Shelley 'RootK XPrv ->
  Integer ->
  Integer ->
  Either String AccountKeys
deriveChildKeys rootKey acctN addrN =
  let acctIndx = indexFromWord32 $ fromInteger acctN
      addrIndx = indexFromWord32 $ fromInteger addrN
   in go acctIndx addrIndx
  where
    go :: Maybe (Index 'Hardened 'AccountK) -> Maybe (Index 'Soft 'PaymentK) -> Either String AccountKeys
    go Nothing _ = Left $ "Invalid Account Index: " ++ show acctN
    go _ Nothing = Left $ "Invalid Address Index: " ++ show addrN
    go (Just accIx) (Just addrIx) =
      let acctK = deriveAccountPrivateKey rootKey accIx
          addrK = deriveAddressPrivateKey acctK S.UTxOExternal addrIx
          stakeK = S.deriveDelegationPrivateKey acctK
       in Right $ AccountKeys addrK stakeK

mkAddress ::
  GYNetworkId ->
  S.Shelley 'PaymentK XPrv -> -- Payment Private Key
  Maybe (S.Shelley 'DelegationK XPrv) -> -- Maybe Stake Private Key
  T.Text
mkAddress nid paymentKey =
  maybe (bech32 mkPaymentAddr) (bech32 . mkDelegationAddr)
  where
    paymentCredential = S.PaymentFromExtendedKey (toXPub <$> paymentKey)
    delegationCredential dKey = S.DelegationFromExtendedKey (toXPub <$> dKey)

    mkPaymentAddr = S.paymentAddress network paymentCredential
    mkDelegationAddr = S.delegationAddress network paymentCredential . delegationCredential

    network =
      case nid of
        GYMainnet -> S.shelleyMainnet
        _ -> S.shelleyTestnet