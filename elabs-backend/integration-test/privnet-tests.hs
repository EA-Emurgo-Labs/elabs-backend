module Main (main) where

import EA.Test.Privnet.OneShotMint qualified
import GeniusYield.Test.Privnet.Setup (makeSetup)
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main = do
  defaultMain $ withResource makeSetup (const mempty) $ \setup -> do
    testGroup
      "privnet Tests"
      [ testGroup
          "OneShotMint"
          [ EA.Test.Privnet.OneShotMint.tests setup
          ]
      ]
