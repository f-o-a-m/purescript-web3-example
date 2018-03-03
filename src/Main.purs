module Main where

import Prelude

import App.CountForm (CountFormProps, countFormClass, ExtendedProvider)
import App.MaterialUI (muiThemeProviderClass)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Address, metamaskProvider, mkAddress, mkHexString)
import Partial.Unsafe (unsafeCrashWith)
import React as R
import ReactDOM (render)
import Network.Ethereum.Uport (AppName(..), UPORT, connect, rinkeby, withAppName, withNetwork, getProvider)
import Control.Monad.Eff.Exception (EXCEPTION)

main :: forall eff. Eff (dom :: DOM, exception :: EXCEPTION, uport :: UPORT | eff) Unit
main = do
  elem <- getElem
  provider <- mkUport
  -- provider <- mkMetamask
  let props = { contractAddress, provider }
  for_ elem $ render $ ui props
  where
    ui :: CountFormProps -> R.ReactElement
    ui props =
      R.createElement muiThemeProviderClass unit
        [ R.createFactory countFormClass props ]

getElem :: forall eff. Eff (dom :: DOM | eff) (Maybe Element)
getElem = do
  win <- window
  doc <- document win
  getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))

mkUport :: forall eff. Eff (exception :: EXCEPTION, uport :: UPORT | eff) ExtendedProvider
mkUport = do
  let
    uportConnect = connect
      $ withAppName (AppName "Web3 example")
      >>> withNetwork rinkeby
  provider <- uportConnect >>= getProvider
  pure { provider, mustSetSender: false }

mkMetamask :: forall eff. Eff (exception :: EXCEPTION | eff) ExtendedProvider
mkMetamask = do
  provider <- metamaskProvider
  pure {provider, mustSetSender: true}

contractAddress :: Address
contractAddress =
  let
    addrMb = do
      str <- hush $ runExcept $ readString simpleStorageAddr
      hex <- mkHexString str
      mkAddress hex
  in case addrMb of
    Just a -> a
    Nothing -> unsafeCrashWith "Make sure environment variable `SIMPLE_STORAGE_ADDRESS` was set to correct hex address during build"

foreign import simpleStorageAddr :: Foreign
