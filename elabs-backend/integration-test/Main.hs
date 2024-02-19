module Main (main) where

import EA.Api.CarbonTests qualified
import EA.Api.WalletTests qualified
import GeniusYield.Test.Privnet.Setup (makeSetup)
import Setup (cleanupSetup)
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main = do
  defaultMain $ withResource makeSetup cleanupSetup $ \setup -> do
    testGroup
      "privnet Tests"
      [ testGroup
          "Wallet"
          [ EA.Api.WalletTests.tests setup
          ]
      , testGroup
          "Mint carbon"
          [ EA.Api.CarbonTests.tests setup
          ]
      ]
