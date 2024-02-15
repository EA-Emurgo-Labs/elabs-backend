module EA.Api.WalletTests (tests) where

import Data.ByteString.Lazy qualified as LB
import EA.Test.Helpers qualified as Helpers
import GeniusYield.Test.Privnet.Setup (Setup)
import Network.HTTP.Types (methodGet)
import Setup (EACtx (..), server, withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertBody, assertStatus, runSession)

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "Wallet tests"
    [ testCaseSteps "Test /api/v0/wallet endpoint" $
        \step -> withEASetup setup step $
          \EACtx {..} -> do
            step "Sending GET request to /api/v0/wallet/1"
            flip runSession (server eaCtxEnv) $ do
              response <-
                Helpers.request
                  methodGet
                  "/api/v0/wallet/1"
                  ""
                  [("Authorization", "token")]
              assertStatus 200 response
              assertBody expectedWalletResponse response
    ]

-- expected result for wallet/1 when called for the first time after db migration
expectedWalletResponse :: LB.ByteString
expectedWalletResponse = "{\"addresses\":[\"004664c8cbe73fd9962b52155fd3f84c70928a705896035f9e1b1333d7c8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"008c29a2611ec3d04529f93ed3bbfae5ae1b82d0951a3447ee3ea95a0dc8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"00bd46fcde0ad5a21e3a069c0592a473370625a3114958e36c49149bfec8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"00043b9e32153e6fd0b1537f636ffe6746a99daa3b73f0e25d866dcfbac8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"001ed899cf19e0d698d0a503c607f2954b2794e390878ec8530a6303b9c8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\"]}"
