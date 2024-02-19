module EA.Api.CarbonTests (tests) where

import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromJust)
import EA (eaAppEnvAuthTokens, eaLiftMaybe, runEAApp)
import EA.Test.Helpers qualified as Helpers
import EA.Wallet (eaGetInternalAddresses)
import GeniusYield.Test.Privnet.Ctx (Ctx (ctxUser2), ctxRunI, submitTx)
import GeniusYield.Test.Privnet.Setup (Setup)
import GeniusYield.TxBuilder (mustHaveOutput)
import GeniusYield.Types (GYTxOut (GYTxOut), valueFromLovelace)
import Network.HTTP.Types (methodPost)
import Setup (EACtx (..), server, withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertStatus, runSession)

{-
curl -X POST 'http://localhost:8081/api/v0/carbon/mint'
  -H "Content-Type: multipart/form-data"
  -F 'file=@"sample-ipfs-file.txt"'
  -F "data={\"userId\":12, \"amount\":100, \"sell\":200}"
-}

-- | Tests for the /api/v0/carbon/mint endpoint
tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "Mint tests"
    [ testCaseSteps "Test /api/v0/carbon/mint endpoint" $
        \step -> withEASetup setup step $
          \EACtx {..} -> do
            step "1. Creating a UTXO on one the internal address"
            flip runSession (server eaCtxEnv) $ do
              void . liftIO $ runEAApp eaCtxEnv $ do
                addrs <- eaGetInternalAddresses True
                (addr, _) <-
                  eaLiftMaybe "No addresses found" $
                    viaNonEmpty head addrs
                let
                  user = ctxUser2 eaCtxCtx
                  tx =
                    mustHaveOutput
                      (GYTxOut addr (valueFromLovelace 5_000_000) Nothing Nothing)
                txBody <- liftIO $ ctxRunI eaCtxCtx user $ return tx
                liftIO $ submitTx eaCtxCtx user txBody

            step "2. Sending POST request to /api/v0/carbon/mint"
            flip runSession (server eaCtxEnv) $ do
              let token =
                    encodeUtf8
                      . fromJust
                      . viaNonEmpty head
                      $ eaAppEnvAuthTokens eaCtxEnv
                  filePath = "sample-ipfs-file.txt"
                  fileContent = "IPFS file content"
                  boundary = "EalabsBackendBoundary"
                  jsonData = "{\"userId\":12, \"amount\":100, \"sell\":200}"
                  body =
                    BL.concat
                      [ "--"
                      , boundary
                      , "\r\n"
                      , "Content-Disposition: form-data; name=\"file\"; filename=\""
                      , filePath
                      , "\"\r\n"
                      , "Content-Type: application/octet-stream\r\n\r\n"
                      , fileContent
                      , "\r\n"
                      , "--"
                      , boundary
                      , "\r\n"
                      , "Content-Disposition: form-data; name=\"data\"\r\n"
                      , "Content-Type: application/json\r\n\r\n"
                      , jsonData
                      , "\r\n"
                      , "--"
                      , boundary
                      , "--"
                      ]
              response <-
                Helpers.request
                  methodPost
                  "/api/v0/carbon/mint"
                  body
                  [ ("Content-Type", "multipart/form-data; boundary=" <> BL.toStrict boundary)
                  , ("Authorization", token)
                  ]
              assertStatus 200 response
    ]
