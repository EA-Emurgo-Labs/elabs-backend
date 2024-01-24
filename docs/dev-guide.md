# Backen developer guide

## The Reader Monad
The Reader Monad passes shared configuration or environment information through a program.
The application defines a core data type, the `EAAppEnv` (see `EA.hs`).
The application code lives in `EAApp`, a wrapper around a Reader monad. 
We call the `asks` function to read any information stored from the environment (`EAAppEnv`) in our application code (`EAApp`).

```haskell
eaLogInfo :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogInfo name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogInfo providers name msg
```

For more information see:
* https://www.fpcomplete.com/blog/readert-design-pattern/
* https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

## Writing logs

We are using the Atlas logging system. You call these functions from within `EAApp`, see `EA.hs`.

```haskell
eaLog :: GYLogNamespace -> GYLogSeverity -> String -> EAApp ()
eaLogDebug :: GYLogNamespace -> String -> EAApp ()
eaLogInfo :: GYLogNamespace -> String -> EAApp ()
eaLogWarning :: GYLogNamespace -> String -> EAApp ()
eaLogError :: GYLogNamespace -> String -> EAApp ()
```

## Writing exceptions

We write `IO` exceptions from within `IO` (during the initialization phase), the `EAApp`, and the DB atomic transaction (to trigger the rollback). For all other places, please apply good functional programming practice, which means no partial functions (errors, exceptions) are allowed.

From within `EApp` (see `EA.hs`), use the following exception functions:
```haskell
eaLiftMaybe :: String -> Maybe a -> EAApp a
eaLiftMaybe' :: (Exception e) => e -> Maybe a -> EAApp a
eaLiftEither :: (a -> String) -> Either a b -> EAApp b
eaLiftEither' :: (Exception e) => (a -> e) -> Either a b -> EAApp b
eaThrow :: (Exception e) => e -> EAApp a
eaCatch :: (Exception e) => EAApp a -> (e -> EAApp a) -> EAApp a
eaHandle :: (Exception e) => (e -> EAApp a) -> EAApp a -> EAApp a
```

These functions will log the exception to the Atlas logging system.

## Writing DB transactions

Use the internal modules to write db transactions (`Internal.Wallet.DB.Sqlite`).
Transactions are written within the `ReaderT SqlBackend m a` monad reader. Everything inside the reader is atomic, and the transaction would roll back in case of an exception.
Because of this property, please don't mix logic within the transaction logic, and be thoughtful about how to use exceptions.

## Writing test
For testing, we are using the Hspec, and at the moment, we are using the automatic spec discovery, which means that when you write a test, you only need to create the spec file.

Suppose you want to test the `Script.hs` module. You would place the `ScriptSpec.hs` file like this:

```
src/
├── EA/
│   └── Script.hs
test/
├── EA/
│   └── ScriptSpec.hs
```

Hspec is similar to unit testing, but it's more declarative.

## Writing integration test
> TODO

## Writing Cardano transactions
Every transaction is written in its own `Tx` module; see example `EA.Tx.OneShotMint`.

They are all in the form of `... -> GYTxSkeleton 'PlutusV2`. Don't use any effect system, just pure functions that are testable.

## Writing new endpoints
We are using the Servant library, which allows us to define the Rest API via type declaration.
The `Api` type inside the `EA.Api` module defines all endpoints.

```haskell
type Api = TxApi :<|> MintApi :<|> WalletApi
```

As you can see, we support `TxApi`, `MintApi`, and `WalletApi`. All these sub-APIs are defined in their respective submodule, `EA.Api.*`.
The data types used for the request or responses are defined in the `EA.Api.Types` module because they are usually shared among other API modules. If not, feel free to keep them inside your API module. Ensure to derive `FromJSON`, `ToSchema` for request types and `ToJSON`, `ToSchema` for respond types.
They can be derived automatically via Haskell Generics. No manual work is needed.

`ToSchema` is needed so we can write a Swagger JSON file for the API documentation that can be handed over to the customer.

## Main function
The main function is `elabs-backend/app/Main.hs`.

Here, we read all the files, initiate everything, and create the `EAppEnv` for our `EAApp`.

We are using the `optparsec` library for the command-line interface.
