module Internal.Wallet.DB.Sqlite (
  createWalletIndexPair,
  getWalletIndexPairs,
  runAutoMigration,
  getUnusedWalletIndexPairs,
) where

import Data.Tagged (Tagged (Tagged))
import Data.Time (getCurrentTime)

import Database.Persist.Sqlite (
  Entity (entityKey, entityVal),
  PersistStoreWrite (insert),
  SelectOpt (Desc),
  SqlBackend,
  fromSqlKey,
  runMigration,
  selectKeysList,
  selectList,
  (==.),
 )

import EA.Api.Types (UserId)

import Internal.Wallet.DB.Schema (
  Address (..),
  EntityField (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

createWalletIndexPair :: (MonadIO m) => UserId -> Int -> ReaderT SqlBackend m ()
createWalletIndexPair userId n = do
  time <- liftIO getCurrentTime
  selectKeysList [] [Desc AccountId]
    >>= \case
      [] -> error "No account record found"
      (key : _) ->
        replicateM_ n $ insert (Address key userId False time)

getUnusedWalletIndexPairs ::
  (MonadIO m) =>
  UserId ->
  Int ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getUnusedWalletIndexPairs userId n = do
  pairs <- getWalletIndexPairs userId False
  when (null pairs) $ createWalletIndexPair userId n
  getWalletIndexPairs userId False

getWalletIndexPairs ::
  (MonadIO m) =>
  UserId ->
  Bool ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs userId used = do
  addrs <- selectList [AddressUsed ==. used, AddressUser ==. userId] []
  return $ map getIndexPair addrs
  where
    getIndexPair addr =
      ( Tagged . fromSqlKey . addressAccountId . entityVal $ addr
      , Tagged . fromSqlKey . entityKey $ addr
      )

runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll
