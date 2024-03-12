module Internal.Wallet.DB.Sql (
  createWalletIndexPair,
  getWalletIndexPairs,
  getWalletIndexPairs',
  getInternalWalletIndexPairs,
  getInternalWalletIndexPairs',
  runAutoMigration,
  createAccount,
  getTokens,
  addToken,
) where

import Data.Tagged (Tagged (Tagged))
import Data.Time (getCurrentTime)
import Database.Persist.Sql (
  Entity (entityKey, entityVal),
  PersistStoreWrite (insert),
  SelectOpt (Desc),
  SqlBackend,
  fromSqlKey,
  runMigration,
  selectKeysList,
  selectList,
  (<-.),
  (==.),
 )
import EA.Api.Types (UserId)
import Internal.Wallet.DB.Schema (
  Account (Account),
  Address (..),
  Auth (..),
  EntityField (..),
  Wallet (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

-- | Create wallet index pairs
createWalletIndexPair ::
  (MonadIO m) =>
  -- | User ID
  Maybe UserId ->
  -- | Number of pairs
  Int ->
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m ()
createWalletIndexPair userId 1 collateral = do
  -- n need to be 1 because how ChangeBlock smart contract v1 is implemented
  time <- liftIO getCurrentTime
  selectKeysList [] [Desc AccountId]
    >>= \case
      [] -> error "No account record found"
      (key : _) ->
        -- replicateM_ n $ do
        do
          addressId <- insert (Address key collateral time)
          void $ insert (Wallet addressId userId time)
createWalletIndexPair _ _ _ =
  error "Only 1 address can be created at a time"

-- | Get wallet index pairs
getWalletIndexPairs ::
  (MonadIO m) =>
  -- | User ID
  UserId ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs userId = do
  wallets <- selectList [WalletUser ==. Just userId] []
  addrs <-
    selectList
      [AddressId <-. (walletAddressId . entityVal <$> wallets)]
      []
  return $ map getIndexPair addrs

-- | Get wallet index pairs, create if not exist
getWalletIndexPairs' ::
  (MonadIO m) =>
  -- | User ID
  UserId ->
  -- | Number of pairs
  Int ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs' userId n = do
  pairs <- getWalletIndexPairs userId
  when (null pairs) $ createWalletIndexPair (Just userId) n False
  getWalletIndexPairs userId

-- | Get internal wallet index pairs
getInternalWalletIndexPairs ::
  (MonadIO m) =>
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs collateral = do
  wallets <- selectList [WalletUser ==. Nothing] []
  addrs <-
    selectList
      [ AddressId <-. (walletAddressId . entityVal <$> wallets)
      , AddressCollateral ==. collateral
      ]
      []
  return $ map getIndexPair addrs

-- | Get internal wallet index pairs, create if not exist
getInternalWalletIndexPairs' ::
  (MonadIO m) =>
  -- | Number of pairs
  Int ->
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs' n collateral = do
  pairs <- getInternalWalletIndexPairs collateral
  when (null pairs) $ createWalletIndexPair Nothing n collateral
  getInternalWalletIndexPairs collateral

-- | Get index pair from address entity
getIndexPair ::
  -- | Address entity
  Entity Address ->
  (Tagged "Account" Int64, Tagged "Address" Int64)
getIndexPair addr =
  ( Tagged . fromSqlKey . addressAccountId . entityVal $ addr
  , Tagged . fromSqlKey . entityKey $ addr
  )

-- | Get all accounts
getAccounts ::
  (MonadIO m) =>
  ReaderT SqlBackend m [Entity Account]
getAccounts = do
  selectList [] []

-- | Create an account if it does not exist
createAccount :: (MonadIO m) => ReaderT SqlBackend m ()
createAccount = do
  accs <- getAccounts
  when (null accs) $ do
    time <- liftIO getCurrentTime
    void $ insert (Account time)

-- | Run auto migration
runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll

-- | Get all tokens
getTokens ::
  (MonadIO m) =>
  ReaderT SqlBackend m [Text]
getTokens = do
  auths :: [Entity Auth] <- selectList [] []
  return $
    authToken . entityVal <$> auths

-- | Add a new token
addToken :: (MonadIO m) => Text -> Text -> ReaderT SqlBackend m ()
addToken token notes = do
  time <- liftIO getCurrentTime
  void $ insert (Auth token notes time)
