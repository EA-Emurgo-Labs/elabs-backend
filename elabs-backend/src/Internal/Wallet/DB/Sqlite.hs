module Internal.Wallet.DB.Sqlite (
  getAddresses,
) where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)

import GeniusYield.Types (GYAddress)

import Database.Persist.Sql (
  Entity (entityVal),
  SqlBackend,
  runMigration,
  selectList,
  (==.),
 )

import EA.Wallet (UserId)

import Internal.Wallet.DB.Schema (
  EntityField (WalletUsed, WalletUser),
  Wallet (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

getAddresses :: (MonadIO m) => UserId -> Bool -> ReaderT SqlBackend m [GYAddress]
getAddresses userId used = do
  addrs <- selectList [WalletUsed ==. used, WalletUser ==. userId] []
  pure $
    map (fromJust . Aeson.decode . fromStrict . walletAddress . entityVal) addrs

_runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
_runAutoMigration = runMigration migrateAll
