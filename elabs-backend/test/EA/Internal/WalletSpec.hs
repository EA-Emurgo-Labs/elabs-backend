module EA.Internal.WalletSpec (spec) where

import Cardano.Mnemonic (mkSomeMnemonic)
import Data.Tagged (Tagged (..))
import GeniusYield.Types (GYNetworkId (GYMainnet))
import Internal.Wallet (deriveAddress, genRootKeyFromMnemonic)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "deriveAddress" $ do
    it "should return an error when the account index is out of range" $ do
      rootKey <-
        either
          (const (error "Invalid mnemonic"))
          (return . genRootKeyFromMnemonic)
          (mkSomeMnemonic @'[15] mnemonic)

      let account = Tagged 2147483648 -- out of range value
      let address = Tagged 0

      deriveAddress GYMainnet rootKey account address `shouldSatisfy` isLeft

    it "should return an error when the address index is out of range" $ do
      rootKey <-
        either
          (const (error "Invalid mnemonic"))
          (return . genRootKeyFromMnemonic)
          (mkSomeMnemonic @'[15] mnemonic)

      let account = Tagged 0
      let address = Tagged 2147483648 -- out of range value
      deriveAddress GYMainnet rootKey account address `shouldSatisfy` isLeft

mnemonic :: [Text]
mnemonic =
  [ "ripple"
  , "scissors"
  , "kick"
  , "mammal"
  , "hire"
  , "column"
  , "oak"
  , "again"
  , "sun"
  , "offer"
  , "wealth"
  , "tomorrow"
  , "wagon"
  , "turn"
  , "fatal"
  ]
