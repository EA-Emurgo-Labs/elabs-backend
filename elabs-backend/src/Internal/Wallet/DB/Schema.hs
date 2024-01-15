{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Wallet.DB.Schema (
  EntityField (WalletUsed, WalletUser),
  Wallet (..),
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
Wallet
  user UserId
  address ByteString
  used Bool default=False
  created UTCTime default=CURRENT_TIME
  deriving Show
|]
