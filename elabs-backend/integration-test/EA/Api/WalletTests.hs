module EA.Api.WalletTests (tests) where

import Data.Maybe (fromJust)
import EA (eaAppEnvAuthTokens)
import EA.Test.Helpers qualified as Helpers
import GeniusYield.Test.Privnet.Ctx (Ctx (ctxUser2))
import GeniusYield.Test.Privnet.Setup (Setup)
import Network.HTTP.Types (methodGet)
import Setup (EACtx (..), server, withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertStatus, runSession)

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "Wallet tests"
    [ testCaseSteps "Test /api/v0/wallet endpoint" $
        \step -> withEASetup ctxUser2 setup step $
          \EACtx {..} -> do
            step "Sending GET request to /api/v0/wallet/1"
            flip runSession (server eaCtxEnv) $ do
              let token =
                    encodeUtf8
                      . fromJust
                      . viaNonEmpty head
                      $ eaAppEnvAuthTokens eaCtxEnv
              response <-
                Helpers.request
                  methodGet
                  "/api/v0/wallet/1"
                  ""
                  [("Authorization", token)]
              assertStatus 200 response
    ]
