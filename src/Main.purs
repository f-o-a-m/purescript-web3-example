module Main where

import Prelude

import App.CountForm (countFormClass)
import App.MaterialUI (muiThemeProviderClass)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Foldable (for_)
import React as R
import ReactDOM (render)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Address, mkHexString, mkAddress)
import Partial.Unsafe (unsafeCrashWith)



main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  elem <- getElem
  for_ elem (render ui)
  where
    ui :: R.ReactElement
    ui =
      R.createElement muiThemeProviderClass unit
        [ R.createFactory countFormClass props ]
    getElem :: Eff (dom :: DOM | eff) (Maybe Element)
    getElem = do
      win <- window
      doc <- document win
      getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))

props :: { contractAddress :: Address }
props = { contractAddress }
  where
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
