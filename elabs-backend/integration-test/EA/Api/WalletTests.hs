module EA.Api.WalletTests (tests) where

import EA (EAAppEnv (..))
import GeniusYield.Test.Privnet.Setup (Setup)
import Setup (EACtx (..), withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertBody, runSession, assertStatus)
import Test.Tasty.Wai qualified as Wai
import EA.Api (apiServer, appApi)
import Servant
    ( hoistServer, serve, Handler(Handler), Application )
import Control.Exception (try)
import qualified Data.ByteString.Lazy as LB

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "Wallet tests"
    [ testCaseSteps "Test /wallet endpoint" $
        \step -> withEASetup setup step $
          \EACtx {..} -> do
            step "Sending GET request to /wallet/1"
            flip runSession (server eaCtxEnv) $ do
              response <- Wai.get "/wallet/1"
              assertStatus 200 response
              assertBody expectedWalletResponse response
    ]

-- expected result for wallet/1 when called for the first time after db migration
expectedWalletResponse :: LB.ByteString
expectedWalletResponse = "{\"addresses\":[\"004664c8cbe73fd9962b52155fd3f84c70928a705896035f9e1b1333d7c8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"008c29a2611ec3d04529f93ed3bbfae5ae1b82d0951a3447ee3ea95a0dc8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"00bd46fcde0ad5a21e3a069c0592a473370625a3114958e36c49149bfec8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"00043b9e32153e6fd0b1537f636ffe6746a99daa3b73f0e25d866dcfbac8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\",\"001ed899cf19e0d698d0a503c607f2954b2794e390878ec8530a6303b9c8b61882f5d8ebe489feed07f191603aedcd40ea743c2bf8fef2d946\"]}"

server :: EAAppEnv -> Application
server env =
  serve appApi
    $ hoistServer appApi (Handler . ExceptT . try)
    $ apiServer env