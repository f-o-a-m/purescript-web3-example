module App.Count where

import Prelude

import Config as Config
import Contracts.SimpleStorage as SimpleStorage
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (CallMode(..), ETH, Metamask, Web3, EventAction(..), event, runWeb3)
import React as R
import React.DOM as D
import Thermite as T

type CountState = {currentCount :: String}

type CountStateProps =
  { statusCallback :: String -> T.EventHandler}

countWatchSpec :: forall eff props . R.ReactSpec CountStateProps CountState (eth :: ETH | eff)
countWatchSpec = (R.spec {currentCount: ""} render) {componentDidMount = monitorCount}
  where
    render :: R.Render CountStateProps CountState (eth :: ETH | eff)
    render this = do
      st <- R.readState this
      pure $ D.h2' [ D.text $ "Current Count: " <> st.currentCount ]

    getInitialState :: R.ComponentWillMount CountStateProps CountState (eth :: ETH | eff)
    getInitialState this = void <<< launchAff $ do
      c <- runWeb3 $ SimpleStorage.count Config.config.simpleStorageAddress Nothing Latest :: Web3 Metamask _ _
      liftEff $ R.transformState this _{currentCount= show c}

    monitorCount :: R.ComponentDidMount CountStateProps CountState (eth :: ETH | eff)
    monitorCount this = void $ do
      props <- R.getProps this
      launchAff <<< runWeb3 $
        event Config.config.simpleStorageAddress $ \(SimpleStorage.NewCount _count) -> do
          _ <- liftEff <<< R.transformState this $ _{currentCount= show _count}
          liftEff $ props.statusCallback "Transaction succeded, enter new count."
          pure TerminateEvent :: ReaderT _ (Web3 Metamask _) _

countWatchClass :: forall props. R.ReactClass CountStateProps
countWatchClass = R.createClass countWatchSpec
