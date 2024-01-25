module EA.Test.Privnet.OneShotMint (tests) where

import Data.List.NonEmpty qualified as NE
import EA.Script (oneShotMintingPolicy)
import EA.Test.Privnet.Helpers (getEaScripts)
import EA.Tx.OneShotMint qualified as Tx
import GeniusYield.Test.Privnet.Ctx (
  Ctx (ctxUser2),
  User (userAddr),
  ctxProviders,
  ctxRunI,
  submitTx,
 )
import GeniusYield.Test.Privnet.Setup (Setup)
import GeniusYield.Types (gyQueryUtxoRefsAtAddress)
import Setup (EACtx (..), withEASetup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "OneShotMint"
    [ testCaseSteps "Minting Token" $ \info -> withEASetup setup info $ \EACtx {..} -> do
        let user = ctxUser2 eaCtxCtx
        utxoRefs <- gyQueryUtxoRefsAtAddress (ctxProviders eaCtxCtx) (userAddr user)
        scripts <- getEaScripts
        let policy = oneShotMintingPolicy (head $ NE.fromList utxoRefs) scripts
        let skeleton = Tx.oneShotMint (userAddr user) (head $ NE.fromList utxoRefs) 1 policy
        txBody <- ctxRunI eaCtxCtx user $ return skeleton
        gyTxId <- submitTx eaCtxCtx user txBody
        info $ "Submitted minting transaction" ++ show gyTxId
    ]
