module EA.Test.Privnet.OneShotMint (tests) where

import Data.List.NonEmpty qualified as NE
import EA.Script (oneShotMintingPolicy)
import EA.Test.Privnet.Helpers (getEaScripts)
import EA.Tx.OneShotMint qualified as Tx
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Internal.Wallet (RootKey, genRootKeyFromMnemonic)
import Cardano.Mnemonic (MkSomeMnemonic(mkSomeMnemonic))
import Test.EA.Helpers (createRootKey)

tests :: IO Setup -> TestTree
tests setup =
  testGroup
    "OneShotMint"
    [ testCaseSteps "Minting Token" $ \info -> withSetup' setup info $ \ctx -> do
        
        let
          _rootKey = createRootKey
          _providers = ctxProviders . ctx2 $ ctx

        let user = ctxUser2 . ctx2 $ ctx
        utxoRefs <- gyQueryUtxoRefsAtAddress (ctxProviders . ctx2 $ ctx) (userAddr user)
        scripts <- getEaScripts
        let policy = oneShotMintingPolicy (head $ NE.fromList utxoRefs) scripts
        let skeleton = Tx.oneShotMint (userAddr user) (head $ NE.fromList utxoRefs) 1 policy
        txBody <- ctxRunI (ctx2 ctx) user $ return skeleton
        gyTxId <- submitTx (ctx2 ctx) user txBody
        info $ "Submitted minting transaction" ++ show gyTxId
    ]


