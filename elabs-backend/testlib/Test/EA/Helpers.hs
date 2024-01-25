module Test.EA.Helpers (
  createRootKey,
  mnemonic,
) where

import Internal.Wallet (RootKey, genRootKeyFromMnemonic)
import Cardano.Mnemonic (MkSomeMnemonic(mkSomeMnemonic))

createRootKey :: RootKey
createRootKey =
  either
    (const (error "Something went wrong with the RootKey creation"))
    genRootKeyFromMnemonic
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