module EA.Test.Privnet.OneShotMint (tests) where

import Data.List.NonEmpty qualified as NE
import EA (oneShotMintingPolicy)
import EA.Test.Privnet.Helpers (getEaScripts)
import EA.Tx.OneShotMint qualified as Tx
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "OneShotMint"
    [ testCaseSteps "Minting Token" $ \info -> withSetup setup info $ \ctx -> do
        let user = ctxUser2 ctx
        utxoRefs <- gyQueryUtxoRefsAtAddress (ctxProviders ctx) (userAddr user)
        scripts <- getEaScripts

        let policy = oneShotMintingPolicy (head $ NE.fromList utxoRefs) scripts
        let skeleton = Tx.oneShotMint (userAddr user) (head $ NE.fromList utxoRefs) 1 policy
        txBody <- ctxRunI ctx user $ return skeleton
        _ <- submitTx ctx user txBody
        info "Submitted minting transaction"
    ]