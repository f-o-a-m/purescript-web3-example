module App.Uport (uportProvider') where


import Prelude

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Uport (AppName(..), UPORT, connect, rinkeby, withAppName, withNetwork, getProvider)
import Network.Ethereum.Web3 (Provider)
import Unsafe.Coerce (unsafeCoerce)

-- NOTE: This is a "quick and dirty hack"
pRef :: Ref (Maybe Provider)
pRef = runPure $ unsafeRunRef $ newRef Nothing

-- NOTE: This is a "quick and dirty hack"
unCool_remove_UPORT_REF :: forall eff. Eff (uport :: UPORT, ref :: REF | eff) ~> Eff eff
unCool_remove_UPORT_REF = unsafeCoerce

-- NOTE: This is a "quick and dirty hack"
uportProvider' :: forall eff. Eff (exception :: EXCEPTION| eff) Provider
uportProvider' = unCool_remove_UPORT_REF uportProvider

uportProvider :: forall eff. Eff (ref :: REF, uport :: UPORT, exception :: EXCEPTION| eff) Provider
uportProvider = do
  mbProvider <- readRef pRef
  case mbProvider of
    Nothing -> do
      provider <- uportConnect >>= getProvider
      writeRef pRef (Just provider)
      pure provider
    Just p ->
      pure p
  where
  uportConnect = connect
    $ withAppName (AppName "Web3 example")
    >>> withNetwork rinkeby




