# Backend developer guide

This documentation should help a developer find how to use the ELabs Backend to create an API for an Emurgo Africa project.

I will explain it by going through all the layers.


## IO layer - Main function
You find the `IO` monad in the main function in `elabs-backend/app/Main.hs`.

The usual pattern with the main is to call all IO functions here, like reading config files, initializing the DB, etc. Then, we create the app environment (`EAppEnv`) with all the information needed to run the backend and store it in the `EApp` reader monad.
This will allow us to ask for any configuration information within the app.

This layer uses the `optparsec` library for the command-line interface.

At the moment, it uses the following commands:

```
> cabal run elabs-backend:app -- --help
elabs-backend - Backend Server

Usage: app [--core-config ARG] [--scripts ARG] [--root-key ARG] COMMAND

  EMURGUO Africa Backend

Available options:
  --core-config ARG        Core config file path (default: "config.json")
  --root-key ARG           Root key file (default: "root.key")
  -h,--help                Show this help text

Available commands:
  run                      Run backend server
  swagger                  Export swagger api
  genrootkey               Root key generation
  internaladdresses        Print internal addresses
  tokens                   Print available auth tokens
  addtoken                 Add new token
```

## `EAApp` layer - The reader monad
That's where we run our application because the `EAApp` has all the necessary information.
The `EAApp` is a reader monad that passes shared configuration or environment information through a program.
The application defines a core data type, the `EAAppEnv` (see `EA.hs`).
We can ask for all the information stored in the `EAAppEnv` with the `asks` function.

The `EAAppEnv` type:
```haskell
data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYNetworkId :: !GYNetworkId
  , eaAppEnvMetrics :: !Metrics
  , eaAppEnvScripts :: !Scripts
  , eaAppEnvSqlPool :: !(Pool SqlBackend)
  , eaAppEnvRootKey :: !RootKey
  }
```

For example, we need the `GYProviders` to call the `gyLogInfo` function. We use the `asks` function to get the providers:

```haskell
eaLogInfo :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogInfo name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogInfo providers name msg
```

### Writing logs

We are using the Atlas logging system. You call these functions from within `EAApp`. See `EA.hs`.

```haskell
eaLog :: GYLogNamespace -> GYLogSeverity -> String -> EAApp ()
eaLogDebug :: GYLogNamespace -> String -> EAApp ()
eaLogInfo :: GYLogNamespace -> String -> EAApp ()
eaLogWarning :: GYLogNamespace -> String -> EAApp ()
eaLogError :: GYLogNamespace -> String -> EAApp ()
```

### `MonadIO`
The `EAApp` is also an `MonadIO`, meaning we can call `IO` functions within this monad. For example, we wish to call the `gyQueryUtxosAtAddresses` function from the Atlas framework.
This function is an `IO` function, and to run it within our `EAApp` function, we use the `liftIO` function:

```haskell
handleOneShotMintByWallet :: WalletParams -> EAApp UnsignedTxResponse
handleOneShotMintByWallet WalletParams {..} = do
  ...
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers usedAddrs
  ...
```

## Business logic layer - When not to use the EAApp layer
We use the `EAApp` whenever we need information about the environment or configurations.
But we should separate logic from `EAApp` whenever possible to make pure functions.
It means that whenever we have business logic that can be separated from the `EApp`, we should write it in a pure Haskell function.

Pure functions are separated from the `IO` and don't have side effects. They are also easier to test, and they can be tested by their properties. Input parameters is mocked more simply than `EAApp` functions.

### Writing Cardano transactions
We write every transaction building logic in its own `Tx` module; see example `EA.Tx.OneShotMint`.

They are all in the form of `... -> GYTxSkeleton 'PlutusV2`. We don't use any effect system, just pure testable functions.

## DB transaction layer

We use the internal modules to write db transactions found here: `Internal.Wallet.DB.Sql`.
We write transactions within the `ReaderT SqlBackend m a` monad reader. Everything inside the reader is atomic, and the transaction would roll back in case of an exception.
Because of this property, we don't mix logic within the transaction logic and are thoughtful about how to use exceptions.

## Writing exceptions

For our backend application, we only write `IO` exceptions from within the `IO` monad (during the initialization phase), the `EAApp`, and the DB atomic transaction (to trigger the rollback).

We apply good functional programming practice for all other functions, which means we do not allow partial functions and exceptions.

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

## Writing test
For testing, we are using the `hspec`, and at the moment, we are using the automatic spec discovery, which means that when you write a test, you only need to create the spec file.

Suppose you want to test the `Script.hs` module. You would place the `ScriptSpec.hs` file like this:

```
src/
├── EA/
│   └── Script.hs
test/
├── EA/
│   └── ScriptSpec.hs
```

`hspec` is similar to unit testing, but it's more declarative.

## Writing new endpoints

We are using the Servant library, which allows us to define the Rest API via type declaration.
The `Api` type inside the `EA.Api` module defines all endpoints.

```haskell
type Api = TxApi :<|> MintApi :<|> WalletApi
```

As you can see, we support `TxApi`, `MintApi`, and `WalletApi`. All these sub-APIs are defined in their respective submodule, `EA.Api.*`.
The data types used for the request or responses are defined in the `EA.Api.Types` module because they are usually shared among other API modules. If not, feel free to keep them inside your API module. Ensure to derive `FromJSON`, `ToSchema` for request types and `ToJSON`, `ToSchema` for respond types.
They can be derived automatically via Haskell Generics. No manual work is needed.

`ToSchema` is needed to write a Swagger JSON file for the API documentation that can be handed over to the customer.

### Writing Swagger documentation
We can automatically generate Swagger documentation with our command line tool:

```
cabal run elabs-backend:app -- swagger
```

## Writing integration test

For the integration test, we are using `tasty`.

Suppose you want to test the `Tx.hs` module. You would place the `TxTests.hs` file like this:

```
src/
├── EA/
│   └── Api/
|       └── Tx.hs
test/
├── EA/
│   └── Api/
|       └── TxTests.hs
```

As you can see in the examples, every test is given the `EACtx` context:

```haskell
data EACtx = EACtx
  { eaCtxCtx :: Ctx
  , eaCtxEnv :: EAAppEnv
  }
```


The `Ctx` is used for the user's wallet, and the `EAAppEnv` is used for testing functions within the `EAApp` context.