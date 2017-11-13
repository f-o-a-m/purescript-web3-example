module Config (config) where

import Prelude
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, mkHexString, mkAddress)
import Partial.Unsafe (unsafePartial)

foreign import simpleStorageAddr :: String

config :: {simpleStorageAddress :: Address }
config = {simpleStorageAddress: unsafePartial fromJust $ mkAddress =<< mkHexString simpleStorageAddr}
