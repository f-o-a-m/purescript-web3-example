module App.Uport (Uport, uport) where


import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Uport (AppName(..), UPORT, connect, rinkeby, withAppName, withNetwork, getProvider)
import Network.Ethereum.Web3 (class IsAsyncProvider, Provider)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


data Uport

uport :: Proxy Uport
uport = Proxy

-- NOTE: This is a "quick and dirty hack"
pRef :: Ref (Maybe Provider)
pRef = runPure $ unsafeRunRef $ newRef Nothing

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


instance providerUportM :: IsAsyncProvider Uport where
  getAsyncProvider = liftAff $ liftEff' $ remove_UPORT_REF uportProvider
    where
      -- TODO add extra parameter of kind `# Effect` to `IsAsyncProvider`
      -- so we don't need to do this.
      remove_UPORT_REF :: forall eff. Eff (uport :: UPORT, ref :: REF | eff) ~> Eff eff
      remove_UPORT_REF = unsafeCoerce



