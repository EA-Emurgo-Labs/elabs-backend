module EA.Test.Helpers (
  createRootKey,
  mnemonic,
  request,
) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Data.ByteString.Lazy qualified as LBS
import Internal.Wallet (RootKey, genRootKeyFromMnemonic)
import Network.HTTP.Types (Header, Method)
import Network.Wai (Request (..))
import Test.Tasty.Wai (
  SRequest (SRequest),
  SResponse,
  Session,
  defaultRequest,
  setPath,
  srequest,
 )

--------------------------------------------------------------------------------

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

-- | Custom request with a body
request ::
  Method ->
  ByteString ->
  LBS.ByteString ->
  [Header] ->
  Session SResponse
request method path body headers =
  let
    req =
      defaultRequest
        { requestMethod = method
        , requestHeaders = headers
        }
    reqWithPath = setPath req path
    sreq = SRequest reqWithPath body
   in
    srequest sreq
