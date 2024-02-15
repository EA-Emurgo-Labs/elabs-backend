module EA.Auth (validToken) where

import Database.Persist.Sql (runSqlPool)
import EA (EAApp, EAAppEnv (..), eaAppEnvSqlPool)
import Internal.Wallet.DB.Sqlite (getTokens)

validToken :: Text -> EAApp Bool
validToken token = do
  tokens <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                getTokens
          )
  return $ token `elem` tokens