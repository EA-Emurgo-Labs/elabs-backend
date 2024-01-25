module EA.Api.MintTests (tests) where

import EA (eaLiftMaybe, runEAApp)
import EA.Api.Types (UserId (UserId))
import EA.Wallet (eaGetAddresses)
import GeniusYield.Test.Privnet.Ctx (Ctx (ctxUser2), ctxRunI, submitTx)
import GeniusYield.Test.Privnet.Setup (Setup)
import GeniusYield.TxBuilder (mustHaveOutput)
import GeniusYield.Types (GYTxOut (GYTxOut), valueFromLovelace)
import Setup (EACtx (..), server, withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (assertStatus, runSession)
import Test.Tasty.Wai qualified as Wai

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "Mint tests"
    [ testCaseSteps "Test /one-shot-mint/1 endpoint" $
        \step -> withEASetup setup step $
          \EACtx {..} -> do
            step "1. Creating first a UTXO on one of the addresses of user 1"
            flip runSession (server eaCtxEnv) $ do
              void . liftIO $ runEAApp eaCtxEnv $ do
                addrs <- eaGetAddresses $ UserId 1
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

            step "2. Sending GET request to /onee-shot-mint/1"
            flip runSession (server eaCtxEnv) $ do
              response <- Wai.post "/one-shot-mint/1" ""
              assertStatus 200 response
    ]
