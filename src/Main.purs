module Main where

import Prelude

import App.App (appClass)
import App.MaterialUI (muiThemeProviderClass)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import React as R
import ReactDOM (render)


main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  elem <- getElem
  for_ elem (render ui)
  where
    ui :: R.ReactElement
    ui =
      R.createElement muiThemeProviderClass unit
        [ R.createFactory appClass unit ]
    getElem :: Eff (dom :: DOM | eff) (Maybe Element)
    getElem = do
      win <- window
      doc <- document win
      getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
