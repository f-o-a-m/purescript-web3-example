module App.Count where

import Prelude

import Config as Config
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff (Milliseconds(..), delay, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (CallMode(..), ETH, EventAction(..), event, metamask, runWeb3)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T

type CountState = {currentCount :: String}

type CountStateProps =
  { statusCallback :: String -> T.EventHandler}

countWatchSpec :: forall eff . R.ReactSpec CountStateProps CountState (eth :: ETH | eff)
countWatchSpec = (R.spec {currentCount: ""} render) { componentWillMount = getInitialState
                                                    , componentDidMount = monitorCount
                                                    }
  where
    render :: R.Render CountStateProps CountState (eth :: ETH | eff)
    render this = do
      st <- R.readState this
      pure $ D.h2 [P.className "count-container"] [ D.text $ "Current Count: " <> st.currentCount ]

    getInitialState :: R.ComponentWillMount CountStateProps CountState (eth :: ETH | eff)
    getInitialState this = void <<< launchAff $ do
      c <- runWeb3 metamask $ SimpleStorage.count Config.config.simpleStorageAddress Nothing Latest
      liftEff $ R.transformState this _{currentCount= show c}

    monitorCount :: R.ComponentDidMount CountStateProps CountState (eth :: ETH | eff)
    monitorCount this = void $ do
      props <- R.getProps this
      launchAff do
        delay (Milliseconds 1000.0)
        void $ runWeb3 metamask $
        event Config.config.simpleStorageAddress $ \(SimpleStorage.CountSet cs) -> do
          _ <- liftEff <<< R.transformState this $ _{currentCount= show cs._count}
          liftEff $ props.statusCallback "Transaction succeded, enter new count."
          pure ContinueEvent

countWatchClass :: R.ReactClass CountStateProps
countWatchClass = R.createClass countWatchSpec
