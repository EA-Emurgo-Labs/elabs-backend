module EA.Test.Privnet.Helpers (getEaScripts) where

import EA.Script (Scripts (Scripts))
import Ply (readTypedScript)

getEaScripts :: IO Scripts
getEaScripts = do
  oneShotPolicyScript <- readTypedScript "scripts.json"
  pure $ Scripts oneShotPolicyScript
