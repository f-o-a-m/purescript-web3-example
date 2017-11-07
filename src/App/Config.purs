module Config where

import Network.Ethereum.Web3 (Address)

foreign import config :: {simpleStorageAddress :: Address }
