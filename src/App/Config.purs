module Config where

import Network.Ethereum.Web3 (Address)

foreign import _simpleStorageAddress :: Address

config :: {simpleStorageAddress :: Address }
config = { simpleStorageAddress: _simpleStorageAddress
         }
