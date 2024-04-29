module Main (main) where

import EA.Api.CarbonTests qualified
import EA.Api.Orders.BuyOrderTests qualified
import EA.Api.Orders.ListOrderTests qualified
import EA.Api.Orders.UpdateOrderSellPriceTests qualified
import EA.Api.WalletTests qualified
import Setup (cleanupSetup, withEASetup)
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main = do
  defaultMain $ withResource withEASetup cleanupSetup $ \eaCtx -> do
    testGroup
      "privnet Tests"
      [ testGroup
          "Wallet"
          [EA.Api.WalletTests.tests eaCtx]
      , testGroup
          "Mint carbon"
          [EA.Api.CarbonTests.tests eaCtx]
      , testGroup
          "Orders Tests"
          [ EA.Api.Orders.BuyOrderTests.tests eaCtx
          , EA.Api.Orders.UpdateOrderSellPriceTests.tests eaCtx
          , EA.Api.Orders.ListOrderTests.tests eaCtx
          ]
      ]
