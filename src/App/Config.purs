module Config (config) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Address, mkHexString, mkAddress)
import Partial.Unsafe (unsafeCrashWith)

foreign import simpleStorageAddr :: Foreign

config :: { simpleStorageAddress :: Address }
config = { simpleStorageAddress }
  where
  simpleStorageAddress = 
    let
      addrMb = do
        str <- hush $ runExcept $ readString simpleStorageAddr
        hex <- mkHexString str
        mkAddress hex
    in case addrMb of
      Just a -> a
      Nothing -> unsafeCrashWith "Make sure environment variable `SIMPLE_STORAGE_ADDRESS` was set to correct hex address during build"
