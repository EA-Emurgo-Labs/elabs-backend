module EA.OrphansSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Database.Persist (PersistField(toPersistValue, fromPersistValue))
import EA.Orphans (GYPubKeyHash)

--------------------------------------------------------------------------------

aPubKeyHash :: GYPubKeyHash
aPubKeyHash = "8c29a2611ec3d04529f93ed3bbfae5ae1b82d0951a3447ee3ea95a0d"

spec :: Spec
spec = do
  describe "PersistField GYPubKeyHash" $ do
    it "should roundtrip toPersistValue . fromPersistValue" $
      fromPersistValue (toPersistValue aPubKeyHash) `shouldBe` Right aPubKeyHash