module EA.Internal.WalletSpec (spec) where

import Data.Tagged (Tagged (..))
import EA.Test.Helpers (createRootKey)
import GeniusYield.Types (GYNetworkId (GYMainnet))
import Internal.Wallet (deriveAddress)
import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), forAll)

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
        forAll arbitrary $ \address -> do
          rootKey <- createRootKey
          let
            result =
              deriveAddress
                GYMainnet
                rootKey
                (Tagged account)
                (Tagged address)
           in
            if account >= 0
              && account <= 2147483647
              && address >= 0
              && address <= 2147483647
              then result `shouldSatisfy` isRight
              else result `shouldSatisfy` isLeft
