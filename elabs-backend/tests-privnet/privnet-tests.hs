module Main (main) where

import qualified EA.Test.Privnet.OneShotMint
import GeniusYield.Test.Privnet.Setup
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
