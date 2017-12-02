module App.CountForm where

import Prelude

import Config as Config
import Contracts.SimpleStorage as SimpleStorage
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Tuple (Tuple(..))
import MaterialUI (EventHandlerOpt(..), UnknownType(..), stringNode)
import MaterialUI.RaisedButton as RaisedButton
import MaterialUI.TextField as TextField
import Network.Ethereum.Web3 (ETH, Address, decimal, metamask, mkAddress, mkHexString, parseBigNumber, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- SimpleStorage Class
--------------------------------------------------------------------------------

type CountFormState =
  { userAddress :: String
  , count :: String
  , errorMessage :: String
  }

initialCountFormState :: CountFormState
initialCountFormState =
    { userAddress: ""
    , count: ""
    , errorMessage: ""
    }

data CountFormAction =
    UpdateAddress String
  | UpdateCount String
  | Submit


type CountFormProps =
  { statusCallback :: String -> T.EventHandler }



countFormSpec :: forall eff . T.Spec (eth :: ETH | eff) CountFormState CountFormProps CountFormAction
countFormSpec = T.simpleSpec performAction render
  where
    render :: T.Render CountFormState CountFormProps CountFormAction
    render dispatch props state _ =
      [ D.div [P.className ""]
         [ D.h3 [P.className "error-message-container"]
           [D.text state.errorMessage]
         , D.form [P.className "count-form"]
           [ D.div'
             [ TextField.textField (  TextField.onChange := (EventHandlerOpt <<< R.handle $ \e ->
                                      dispatch (UpdateAddress (unsafeCoerce e).target.value))
                                 <> TextField.hintText := stringNode "0xdeadbeef"
                                 <> TextField.fullWidth := true
                                 <> TextField.floatingLabelText := stringNode "User Address"
                                 <> TextField.value := UnknownType (toForeign state.userAddress)
                                 ) []
             ]
           , D.div'
             [ TextField.textField (  TextField.onChange := (EventHandlerOpt <<< R.handle $ \e ->
                                      dispatch (UpdateCount (unsafeCoerce e).target.value))
                                 <> TextField.hintText := stringNode "123"
                                 <> TextField.fullWidth := true
                                 <> TextField.floatingLabelText := stringNode "New Count"
                                 <> TextField.value := UnknownType (toForeign state.count)
                                 ) []
             ]
            , D.div [P.className "submit-button-container"]
              [ RaisedButton.raisedButton ( RaisedButton.onClick := (EventHandlerOpt <<< R.handle $ \_ -> dispatch Submit)
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
        Left err -> void <<< T.modifyState $ _{errorMessage = err}
        Right (Tuple sender count) -> void do
          txHash <- lift $ runWeb3 metamask $
            SimpleStorage.setCount (Just $ Config.config.simpleStorageAddress) sender count
          lift <<< unsafeCoerceAff <<< liftEff $ props.statusCallback $ "Transaction Hash: " <> show txHash
          T.modifyState $ _{ errorMessage = "", count = ""}

    performAction (UpdateAddress addr) _ _ = void <<< T.modifyState $ _{userAddress = addr}

    performAction (UpdateCount n) _ _ = void <<< T.modifyState $ _{count = n}

countFormClass :: R.ReactClass CountFormProps
countFormClass =
    let {spec} = T.createReactSpec countFormSpec (const $ pure initialCountFormState)
    in R.createClass (spec {componentWillMount = completeAddressField})
  where
    completeAddressField this = void <<< launchAff $ do
      maddr <- getUserAddress
      liftEff $ maybe (pure unit) (\a -> void $ R.transformState this $ _{userAddress = show a}) maddr

--------------------------------------------------------------------------------

getUserAddress :: forall eff . Aff (eth :: ETH | eff) (Maybe Address)
getUserAddress = do
  accounts <- runWeb3 metamask $ eth_getAccounts
  pure $ accounts !! 0
