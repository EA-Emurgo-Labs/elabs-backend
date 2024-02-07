module EA.Api (
  appApi,
  apiServer,
  apiSwagger,
) where

import Data.Swagger (Swagger)
import EA (EAApp)
import EA.Api.Carbon (CarbonApi, handleCarbonMint)
import EA.Api.Mint (
  MintApi,
  handleOneShotMintByUserId,
  handleOneShotMintByWallet,
 )
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Wallet (WalletApi, handleWalletApi)
import Servant (HasServer (ServerT), type (:<|>) (..))
import Servant.Swagger (toSwagger)

--------------------------------------------------------------------------------

type Api =
  TxApi :<|> MintApi :<|> WalletApi :<|> CarbonApi

apiSwagger :: Swagger
apiSwagger = toSwagger appApi

appApi :: Proxy Api
appApi = Proxy

apiServer :: ServerT Api EAApp
apiServer =
  handleTxApi
    :<|> ( handleOneShotMintByWallet
            :<|> handleOneShotMintByUserId
         )
    :<|> handleWalletApi
    :<|> handleCarbonMint
