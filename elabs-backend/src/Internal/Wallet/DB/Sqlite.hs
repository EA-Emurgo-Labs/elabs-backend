module Internal.Wallet.DB.Sqlite (
  getAddresses,
  insertUnusedAddresses,
  runAutoMigration,
) where

import Data.Time (getCurrentTime)

import Database.Persist.Sqlite (
  Entity (entityVal),
  PersistStoreWrite (insert),
  SqlBackend,
  runMigration,
  selectList,
  (==.),
 )

import EA.Api.Types (UserId)

import Internal.Wallet.DB.Schema (
  EntityField (WalletUsed, WalletUser),
  Wallet (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

insertUnusedAddresses ::
  (MonadIO m) =>
  UserId ->
  [ByteString] ->
  ReaderT SqlBackend m ()
insertUnusedAddresses userid addresses = do
  time <- liftIO getCurrentTime
  forM_ addresses (\addr -> insert $ Wallet userid addr False time)

getAddresses ::
  (MonadIO m) =>
  UserId ->
  Bool ->
  ReaderT SqlBackend m [ByteString]
getAddresses userId used = do
  addrs <- selectList [WalletUsed ==. used, WalletUser ==. userId] []
  pure $
    map (walletAddress . entityVal) addrs

runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll
