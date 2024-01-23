module EA.Internal.WalletSpec (spec) where

import Data.Tagged (Tagged (..))

import Cardano.Mnemonic (mkSomeMnemonic)
import GeniusYield.Types (GYNetworkId (GYMainnet))

import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), forAll)

import Internal.Wallet (RootKey, deriveAddress, genRootKeyFromMnemonic)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "deriveAddress" $ do
    prop
      ( "Should return Right when account and address indices are within "
          <> "[0, 2147483647], and Left otherwise"
      )
      $ forAll arbitrary
      $ \account ->
        forAll arbitrary $ \address ->
          let
            result =
              deriveAddress
                GYMainnet
                createRootKey
                (Tagged account)
                (Tagged address)
           in
            if account >= 0
              && account <= 2147483647
              && address >= 0
              && address <= 2147483647
              then result `shouldSatisfy` isRight
              else result `shouldSatisfy` isLeft

createRootKey :: RootKey
createRootKey =
  either
    (const (error "Something went wrong with the RootKey creation"))
    genRootKeyFromMnemonic
    (mkSomeMnemonic @'[15] mnemonic)

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
