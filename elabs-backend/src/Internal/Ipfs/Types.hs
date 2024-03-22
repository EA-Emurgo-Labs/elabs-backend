module Internal.Ipfs.Types (
  IpfsListResponse (..),
  IpfsAddResponse (..),
  IpfsPin (..),
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger

--------------------------------------------------------------------------------

data IpfsListResponse = IpfsListResponse
  { time_created :: !Integer
  , time_pinned :: !Integer
  , ipfs_hash :: !Text
  , size :: !Text
  , state :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

data IpfsAddResponse = IpfsAddResponse
  { name :: !Text
  , ipfs_hash :: !Text
  , size :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

data IpfsPin = IpfsPin
  { ipfs_hash :: !Text
  , state :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)
