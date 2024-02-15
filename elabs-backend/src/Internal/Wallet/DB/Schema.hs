{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Wallet.DB.Schema (
  EntityField (..),
  Account (..),
  Address (..),
  Auth (..),
  migrateAll,
) where

import Data.Time (UTCTime)
import Database.Persist (
  EntityField,
  FieldDef (
    fieldAttrs,
    fieldCascade,
    fieldComments,
    fieldDB,
    fieldGenerated,
    fieldHaskell,
    fieldIsImplicitIdColumn,
    fieldReference,
    fieldSqlType,
    fieldStrict,
    fieldType
  ),
 )
import Database.Persist.Sqlite (
  BackendKey (SqlBackendKey),
 )
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )

import EA.Api.Types (UserId (..))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Account
  created UTCTime default=CURRENT_TIME
  deriving Show

Address
  accountId AccountId
  user UserId Maybe
  created UTCTime default=CURRENT_TIME
  deriving Show

Auth
  token Text
  created UTCTime default=CURRENT_TIME
  deriving Show

|]
