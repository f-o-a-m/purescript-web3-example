module UportUtils where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Promise as Promise
import Network.Ethereum.Uport as Uport

foreign import requestCredentials :: Uport.Connect -> Promise.Promise {address :: String}

dumbRequestCredentials :: forall eff . Uport.Connect -> Aff (uport :: Uport.UPORT, console :: CONSOLE | eff) Unit
dumbRequestCredentials connect  = do
  creds <- Promise.toAff $ requestCredentials connect
  log creds.address
