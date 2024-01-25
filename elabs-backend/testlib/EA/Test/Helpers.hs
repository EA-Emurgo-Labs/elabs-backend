module EA.Test.Helpers (
  createRootKey,
  mnemonic,
) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Internal.Wallet (RootKey, genRootKeyFromMnemonic)

createRootKey :: IO RootKey
createRootKey =
  either
    (const (error "Something went wrong with the RootKey creation"))
    (return . genRootKeyFromMnemonic)
    (mkSomeMnemonic @'[15] mnemonic)

mnemonic :: [Text]
mnemonic =
  [ "ripple"
  , "scissors"
  , "kick"
  , "mammal"
  , "hire"
  , "column"
  , "oak"
  , "again"
  , "sun"
  , "offer"
  , "wealth"
  , "tomorrow"
  , "wagon"
  , "turn"
  , "fatal"
  ]
