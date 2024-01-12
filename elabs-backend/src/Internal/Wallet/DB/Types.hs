{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Wallet.DB.Types where

import Database.Persist (PersistField (..), PersistValue)
import Database.Persist.Sql (PersistFieldSql (..))

import EA.Wallet (UserId (..))

persistUserId :: UserId -> PersistValue
persistUserId = toPersistValue . unUserId

unPersistUserId :: PersistValue -> Either Text UserId
unPersistUserId = fmap UserId . fromPersistValue

instance PersistField UserId where
  toPersistValue = persistUserId
  fromPersistValue = unPersistUserId

instance PersistFieldSql UserId where
  sqlType _ = sqlType (Proxy @Word64)
