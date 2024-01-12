module Internal.Wallet.DB.Sqlite (
  getAddresses
) where

import Data.Maybe (fromJust)
import qualified Data.Aeson as Aeson

import GeniusYield.Types (GYAddress)

import Database.Persist.Sql (
	SqlBackend, selectList, Entity (entityVal), (==.), runMigration)

import EA.Wallet (UserId)

import Internal.Wallet.DB.Schema (
        Wallet(..), EntityField (WalletUsed, WalletUser), migrateAll)

--------------------------------------------------------------------------------

getAddresses :: MonadIO m => UserId -> Bool -> ReaderT SqlBackend m [GYAddress]
getAddresses userId used = do
  addrs <- selectList [WalletUsed ==. used, WalletUser ==. userId] []
  pure $
	  map (fromJust . Aeson.decode . fromStrict . walletAddress . entityVal) addrs


_runAutoMigration :: MonadIO m => ReaderT SqlBackend m ()
_runAutoMigration = runMigration migrateAll
