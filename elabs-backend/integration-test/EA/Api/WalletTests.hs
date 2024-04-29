module EA.Api.WalletTests (tests) where

import EA.Test.Helpers qualified as Helpers
import Network.HTTP.Types (methodGet)
import Setup (EACtx (..), server, withEaCtx)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertStatus, runSession)

tests :: IO EACtx -> TestTree
tests eaCtx =
  testGroup
    "Wallet tests"
    [ testCaseSteps "Test /api/v0/wallet endpoint" $
        \step -> withEaCtx eaCtx $
          \EACtx {..} -> do
            step "Sending GET request to /api/v0/wallet/1"
            flip runSession (server eaCtxEnv) $ do
              let token = encodeUtf8 eaCtxToken
              response <-
                Helpers.request
                  methodGet
                  "/api/v0/wallet/1"
                  ""
                  [("Authorization", token)]
              assertStatus 200 response
    ]
