module EA.Api.Orders.ListOrderTests (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import EA.Script.Marketplace (MarketplaceInfo)
import EA.Test.Helpers qualified as Helpers
import Network.HTTP.Types (methodGet)
import Setup (EACtx (..), server, withEaCtx)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (SResponse (simpleBody), assertStatus, runSession)

tests :: IO EACtx -> TestTree
tests eaCtx =
  testGroup
    "Order Buy Tests"
    [ testCaseSteps "Test /api/v0/orders endpoint" $
        \step -> withEaCtx eaCtx $
          \EACtx {..} -> do
            -- Sending some fund to the buyer
            step "Sending GET request to /api/v0/orders"

            resp <- flip runSession (server eaCtxEnv) $ do
              let token = encodeUtf8 eaCtxToken
              response <-
                Helpers.request
                  methodGet
                  "/api/v0/orders/"
                  ""
                  [ ("Content-type", "Application/json")
                  , ("Authorization", token)
                  ]

              assertStatus 200 response
              pure response

            void $ parseMinfoResp (simpleBody resp)
    ]

parseMinfoResp :: BSL.ByteString -> IO [MarketplaceInfo]
parseMinfoResp body = do
  case Aeson.eitherDecode body of
    Left err -> fail err
    Right infos -> pure infos