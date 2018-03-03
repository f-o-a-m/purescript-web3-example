module App.CountForm where

import Prelude

import App.Uport (uportProvider')
import Contracts.SimpleStorage as SimpleStorage
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, launchAff, liftEff', Milliseconds(..), delay)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Array (index)
import Data.Either (Either(..), hush)
import Data.Foreign (toForeign)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Tuple (Tuple(..))
import MaterialUI (EventHandlerOpt(..), UnknownType(..), stringNode)
import MaterialUI.RaisedButton as RaisedButton
import MaterialUI.TextField as TextField
import Network.Ethereum.Web3 (ChainCursor(..), ETH, EventAction(..), _to, defaultTransactionOptions, event, eventFilter, forkWeb3, runWeb3, Address, _from, decimal, mkAddress, mkHexString, parseBigNumber, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- SimpleStorage Class
--------------------------------------------------------------------------------

type CountFormState =
  { userAddress :: String
  , count :: String
  , errorMessage :: String
  , currentCount :: String
  , status :: String
  }

initialCountFormState :: CountFormState
initialCountFormState =
    { userAddress: ""
    , count: ""
    , errorMessage: ""
    , currentCount: ""
    , status: "Please enter a count."
    }

data CountFormAction =
    UpdateAddress String
  | UpdateCount String
  | Submit


type CountFormProps =
  { contractAddress :: Address
  }



countFormSpec :: forall eff . T.Spec (eth :: ETH | eff) CountFormState CountFormProps CountFormAction
countFormSpec = T.simpleSpec performAction render
  where
    render :: T.Render CountFormState CountFormProps CountFormAction
    render dispatch props state _ =
      [ D.div [P.className "status-bar"] [D.text state.status]
      , D.div' 
          [ D.h2 [P.className "count-container"] [ D.text $ "Contract address: " <> show props.contractAddress]
          , D.h2 [P.className "count-container"] [ D.text $ "Current Count: " <> state.currentCount ]
          ]
      , D.div [P.className ""]
         [ D.h3 [P.className "error-message-container"]
           [D.text state.errorMessage]
         , D.form [P.className "count-form"]
           [ D.div'
             [ TextField.textField (  TextField.onChange := (EventHandlerOpt $ R.handle $ \e ->
                                                                          dispatch (UpdateAddress (unsafeCoerce e).target.value))
                                 <> TextField.hintText := stringNode "0xdeadbeef"
                                 <> TextField.fullWidth := true
                                 <> TextField.floatingLabelText := stringNode "User Address"
                                 <> TextField.value := UnknownType (toForeign state.userAddress)
                                 ) []
             ]
           , D.div'
             [ TextField.textField (  TextField.onChange := (EventHandlerOpt $ R.handle $ \e ->
                                      dispatch (UpdateCount (unsafeCoerce e).target.value))
                                 <> TextField.hintText := stringNode "123"
                                 <> TextField.fullWidth := true
                                 <> TextField.floatingLabelText := stringNode "New Count"
                                 <> TextField.value := UnknownType (toForeign state.count)
                                 ) []
             ]
            , D.div [P.className "submit-button-container"]
              [ RaisedButton.raisedButton ( RaisedButton.onClick := (EventHandlerOpt $ R.handle $ \_ -> dispatch Submit)
                                          <> RaisedButton.backgroundColor := "#2196F3"
                                          <> RaisedButton.fullWidth := true
                                        ) [ D.div
                                            [ P.className "submit-button-text" ]
                                            [ D.text "Submit" ]
                                          ]
              ]
            ]
         ]
      ]

    performAction :: T.PerformAction (eth :: ETH | eff) CountFormState CountFormProps CountFormAction
    performAction Submit props st = do
      let args = do
            addr <- note "Please enter a valid ethereum address" $ mkAddress =<< mkHexString st.userAddress
            count <- note "Please enter a valid uint256." $ uIntNFromBigNumber =<<  parseBigNumber decimal st.count
            pure $ Tuple addr count
      case args of
        Left err -> void $ T.modifyState _{errorMessage = err}
        Right (Tuple sender count) -> void do
          p <- lift $ liftEff' uportProvider'
          txHash <- lift $ runWeb3 p $ do
            let txOpts = defaultTransactionOptions # _from .~ Just sender
                                                   # _to .~ Just props.contractAddress
            SimpleStorage.setCount txOpts { _count : count }
          T.modifyState _{ errorMessage = "", count = "", status = "Transaction Hash: " <> show txHash}

    performAction (UpdateAddress addr) _ _ = void $ T.modifyState _{userAddress = addr}

    performAction (UpdateCount n) _ _ = void $ T.modifyState _{count = n}

countFormClass :: R.ReactClass CountFormProps
countFormClass =
    let {spec} = T.createReactSpec countFormSpec (const $ pure initialCountFormState)
    in R.createClass (spec { componentWillMount = componentWillMount })
  where
  componentWillMount :: forall eff. R.ComponentWillMount CountFormProps CountFormState (eth :: ETH | eff)
  componentWillMount this = do 
    completeAddressField this
    getInitialState this
    monitorCount this
  
  completeAddressField :: forall eff. R.ComponentWillMount CountFormProps CountFormState ( eth :: ETH | eff)
  completeAddressField this = void $ launchAff do
    maddr <- getUserAddress
    liftEff $ maybe (pure unit) (\a -> void $ R.transformState this _{userAddress = show a}) maddr
  
  getInitialState :: forall eff. R.ComponentWillMount CountFormProps CountFormState (eth :: ETH | eff)
  getInitialState this = void $ launchAff $ do
    props <- liftEff $ R.getProps this
    let txOpts = defaultTransactionOptions # _to .~ Just props.contractAddress
    p <- liftEff' uportProvider'
    c <- runWeb3 p $ SimpleStorage.count txOpts Latest
    liftEff $ R.transformState this _{currentCount= show c}
  
  monitorCount :: forall eff. R.ComponentWillMount CountFormProps CountFormState (eth :: ETH | eff)
  monitorCount this = void $ launchAff $ do
    props <- liftEff $ R.getProps this
    delay (Milliseconds 1000.0)
    p <- liftEff' uportProvider'
    void $ forkWeb3 p $ do
      let fltr = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) props.contractAddress
      event fltr $ \(SimpleStorage.CountSet cs) -> do
        _ <- liftEff $ R.transformState this _{currentCount = show cs._count, status = "Transaction succeded, enter new count."}
        pure ContinueEvent

--------------------------------------------------------------------------------

getUserAddress :: forall eff . Aff (eth :: ETH | eff) (Maybe Address)
getUserAddress = do
  p <- liftEff' uportProvider'
  accounts <- map hush $ runWeb3 p $ eth_getAccounts
  pure $ accounts >>= flip index 0
