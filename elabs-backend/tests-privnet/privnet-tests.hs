module Main (main) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main = do
  defaultMain $ withResource makeSetup (const mempty) $ \_setup -> do
    testGroup "privnet Test" []
