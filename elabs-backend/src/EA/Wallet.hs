{-# OPTIONS_GHC -Wno-deprecations #-}

module EA.Wallet (
  eaSignGYTxBody,
  eaGetCollateral,
  eaGetUnusedAddresses,
  eaGetUsedAddresses,
  eaCreateAddresses,
) where

import Data.Aeson qualified as Aeson

import GeniusYield.Types (
  GYAddress,
  GYTx,
  GYTxBody,
  GYTxOutRef,
  unsafeAddressFromText,
 )

import Database.Persist.Sql (runSqlPool)

import EA (EAApp, eaAppEnvSqlPool, eaLiftMaybe)
import EA.Api.Types (UserId)

import Internal.Wallet.DB.Sqlite (getAddresses, insertUnusedAddresses)

--------------------------------------------------------------------------------

eaGetUnusedAddresses :: UserId -> EAApp [GYAddress]
eaGetUnusedAddresses = eaGetAddresses False

eaGetUsedAddresses :: UserId -> EAApp [GYAddress]
eaGetUsedAddresses = eaGetAddresses True

eaGetAddresses :: Bool -> UserId -> EAApp [GYAddress]
eaGetAddresses used userid = do
  addresses <-
    asks eaAppEnvSqlPool
      >>= ( liftIO
              . runSqlPool
                (getAddresses userid used)
          )
  mapM (eaLiftMaybe "Decoding error" . Aeson.decode . fromStrict) addresses

eaCreateAddresses :: UserId -> EAApp [GYAddress]
eaCreateAddresses userid = do
  asks eaAppEnvSqlPool
    >>= ( liftIO
            . runSqlPool
              (insertUnusedAddresses userid [toStrict $ Aeson.encode addr])
        )
  -- TODO: don query again
  eaGetUnusedAddresses userid
  where
    -- TODO: test address
    addr =
      unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"

{-
  use esqualito

   get addresses
     -> select * from  left join, if empty, create cache
     -> if empty creat new addresses
     -> repeat until we have all addresses

-}

-- TODO:
eaGetCollateral :: EAApp (Maybe (GYTxOutRef, Bool))
eaGetCollateral = undefined

eaSignGYTxBody :: GYTxBody -> EAApp GYTx
eaSignGYTxBody = undefined
