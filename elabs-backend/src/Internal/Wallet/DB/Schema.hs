{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Internal.Wallet.DB.Schema where

import Database.Persist
    ( FieldDef(fieldIsImplicitIdColumn, fieldHaskell, fieldDB,
               fieldType, fieldSqlType, fieldAttrs, fieldStrict, fieldReference,
               fieldCascade, fieldComments, fieldGenerated) )
import Database.Persist.Sqlite
    ( BackendKey(SqlBackendKey) )
import Database.Persist.TH
    ( sqlSettings, share, persistLowerCase, mkPersist, mkMigrate )
import Data.Time ( UTCTime )

import EA.Wallet (UserId(..))

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Wallet
  user UserId
  address ByteString
  used Bool default=False
  created UTCTime default=CURRENT_TIME
  deriving Show
|]
