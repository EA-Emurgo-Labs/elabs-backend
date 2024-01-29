module Internal.Wallet.DB.Sqlite (
  createWalletIndexPair,
  getWalletIndexPairs,
  getWalletIndexPairs',
  getInternalWalletIndexPairs,
  getInternalWalletIndexPairs',
  runAutoMigration,
  createAccount,
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
  Account (Account),
  Address (..),
  EntityField (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

createWalletIndexPair :: (MonadIO m) => Maybe UserId -> Int -> ReaderT SqlBackend m ()
createWalletIndexPair userId n = do
  time <- liftIO getCurrentTime
  selectKeysList [] [Desc AccountId]
    >>= \case
      [] -> error "No account record found"
      (key : _) ->
        replicateM_ n $ insert (Address key userId time)

getWalletIndexPairs ::
  (MonadIO m) =>
  UserId ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs userId = do
  addrs <- selectList [AddressUser ==. Just userId] []
  return $ map getIndexPair addrs

getWalletIndexPairs' ::
  (MonadIO m) =>
  UserId ->
  Int ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs' userId n = do
  pairs <- getWalletIndexPairs userId
  when (null pairs) $ createWalletIndexPair (Just userId) n
  getWalletIndexPairs userId

getInternalWalletIndexPairs ::
  (MonadIO m) =>
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs = do
  addrs <- selectList [AddressUser ==. Nothing] []
  return $ map getIndexPair addrs

getInternalWalletIndexPairs' ::
  (MonadIO m) =>
  Int ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs' n = do
  pairs <- getInternalWalletIndexPairs
  when (null pairs) $ createWalletIndexPair Nothing n
  getInternalWalletIndexPairs

getIndexPair ::
  Entity Address ->
  (Tagged "Account" Int64, Tagged "Address" Int64)
getIndexPair addr =
  ( Tagged . fromSqlKey . addressAccountId . entityVal $ addr
  , Tagged . fromSqlKey . entityKey $ addr
  )

createAccount :: (MonadIO m) => ReaderT SqlBackend m ()
createAccount = do
  time <- liftIO getCurrentTime
  void $ insert (Account time)

runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll
