module EA.CommonException (CommonException (..)) where

import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (toApiError))
import GeniusYield.Types (GYAddress, GYPubKeyHash)
import Network.HTTP.Types (status400)

data CommonException
  = EaNoUtxo {cmnExpEaAddressUtxo :: GYAddress}
  | EaNoCollateral
  | EaInvalidAddres {cmnExpEaAddress :: GYAddress}
  | EaCannotDecodeAddress {cmnExpEaPubKey :: GYPubKeyHash}
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError CommonException where
  toApiError (EaNoUtxo address) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_UTXO_FOUND"
      , gaeMsg = "No UTxO found for address: " <> show address
      }
  toApiError EaNoCollateral =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_COLLATERAL_FOUND"
      , gaeMsg = "No collateral found "
      }
  toApiError (EaInvalidAddres address) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_ADDRESS"
      , gaeMsg = "Invalid address: " <> show address
      }
  toApiError (EaCannotDecodeAddress pubKey) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "CANNOT_DECODE_ADDRESS"
      , gaeMsg = "Cannot decode address for pub key: " <> show pubKey
      }