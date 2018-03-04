module App.CountForm where

import Prelude

import Contracts.SimpleStorage as SimpleStorage
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, Error, Milliseconds(..), delay, forkAff, joinFiber, launchAff, try)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Array (index)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Foreign (toForeign)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options ((:=))
import Data.Options as Options
import MaterialUI (EventHandlerOpt(..), UnknownType(..), stringNode)
import MaterialUI.RaisedButton as RaisedButton
import MaterialUI.TextField (TextFieldOption)
import MaterialUI.TextField as TextField
import Network.Ethereum.Web3 (type (:&), Address, CallError, ChainCursor(..), D2, D5, D6, ETH, EventAction(..), Provider, UIntN, Web3Error, _from, _to, decimal, defaultTransactionOptions, event, eventFilter, forkWeb3, mkAddress, mkHexString, parseBigNumber, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Types.Types (HexString)
import Partial.Unsafe (unsafeCrashWith)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- SimpleStorage Class
--------------------------------------------------------------------------------

type Count = (UIntN (D2 :& D5 :& D6))
type CountFormState =
  { userAddress :: InputVal Address
  , count :: InputVal Count
  , currentCount :: Maybe (Either CountFetchError Count)
  , status :: Maybe SubmitStatus
  }

data SubmitStatus
  = FailedToCreateTransaction Web3Error
  | TransactionWasCreated HexString
  | NewValueHasArrived

data CountFetchError
  = FailedToFetchCountWithWeb3Error Web3Error
  | FailedToFetchCountWithCallError CallError

type InputVal a = { val :: Either String a, input :: String }

emptyInputVal :: forall a. InputVal a
emptyInputVal = {val: Left "", input: ""}

initialCountFormState :: CountFormState
initialCountFormState =
  { userAddress: emptyInputVal
  , count: emptyInputVal
  , currentCount: Nothing
  , status: Nothing
  }

data CountFormAction
  = UpdateAddress String
  | UpdateCount String
  | Submit

type ExtendedProvider =
  { provider :: Provider
  , mustSetSender :: Boolean
  }

type CountFormProps =
  { contractAddress :: Address
  , provider :: ExtendedProvider
  }

strVal :: String -> UnknownType
strVal = toForeign >>> UnknownType

inputValToValueOpt :: forall a. InputVal a -> Options.Options TextFieldOption
inputValToValueOpt iv = TextField.value := strVal iv.input

inputValToErrorOpt :: forall a. InputVal a -> Options.Options TextFieldOption
inputValToErrorOpt iv = case iv.val of
  Left err -> TextField.errorText := stringNode err
  Right _ -> mempty

inputValToOpts :: forall a. InputVal a -> Options.Options TextFieldOption
inputValToOpts = inputValToValueOpt <> inputValToErrorOpt
  
listenToInput :: forall action. (action -> T.EventHandler) -> (String -> action) -> Options.Options TextFieldOption
listenToInput dispatch ctr =
  TextField.onChange := (EventHandlerOpt $ R.handle $ \e -> dispatch $ ctr (unsafeCoerce e).target.value)

submitDisabled :: CountFormState -> CountFormProps -> Boolean
submitDisabled st props = case st.count.val of
  Left _ -> true
  Right _ | not props.provider.mustSetSender -> false
  Right _ | otherwise -> case st.userAddress.val of
    Right _ -> false
    Left _ -> true

countFormSpec :: forall eff . T.Spec (eth :: ETH | eff) CountFormState CountFormProps CountFormAction
countFormSpec = T.simpleSpec performAction render
  where
    render :: T.Render CountFormState CountFormProps CountFormAction
    render dispatch props state _ =
      [ D.h2
          [ P.className "count-container" ]
          [ D.text $ "Contract address: " <> show props.contractAddress]
      , D.h2
          [ P.className "count-container" ]
          [ D.text $ "Current Count: " <> case state.currentCount of 
              Nothing -> "loading ..."
              Just (Left err) -> "error occurred while fetching count, retrying ..."
              Just (Right c) -> show c
          ]
      , D.form
          [ P.className "count-form"]
          [ D.div'
              [ if props.provider.mustSetSender then
                  TextField.textField
                    (listenToInput dispatch UpdateAddress
                      <> TextField.hintText := stringNode "0xdeadbeef"
                      <> TextField.fullWidth := true
                      <> TextField.floatingLabelText := stringNode "User Address *"
                      <> inputValToOpts state.userAddress
                    ) []
                 else
                   D.text "Sender of the transaction will be set by the provider"
              ]
          , D.div'
              [ TextField.textField
                  ( listenToInput dispatch UpdateCount
                    <> TextField.hintText := stringNode "123"
                    <> TextField.fullWidth := true
                    <> TextField.floatingLabelText := stringNode "New Count *"
                    <> inputValToOpts state.count
                  ) []
              ]
          , D.div
              [ P.className "submit-button-container" ]
              [ RaisedButton.raisedButton
                  ( RaisedButton.onClick := (EventHandlerOpt $ R.handle $ \_ -> dispatch Submit)
                    <> RaisedButton.backgroundColor := "#2196F3"
                    <> RaisedButton.fullWidth := true
                    <> RaisedButton.disabled := submitDisabled state props
                  ) [ D.div
                      [ P.className "submit-button-text" ]
                      [ D.text "Submit" ]
                    ]
              ]
          ]
      , D.div [P.className "status-bar"]
        [ D.text case state.status of
            Nothing -> "Please enter a count."
            Just (FailedToCreateTransaction err) -> "Failed to create transaction, try again" -- TODO show err
            Just (TransactionWasCreated txHash) -> "Transaction was created with Hash: " <> show txHash
            Just NewValueHasArrived -> "Transaction succeded, enter new count."
        ]
      ]

    performAction :: T.PerformAction (eth :: ETH | eff) CountFormState CountFormProps CountFormAction
    performAction Submit props st = do
      for_ st.count.val \count -> do
        let
          txOpts = defaultTransactionOptions
            # _to .~ Just props.contractAddress
            # case st.userAddress.val of
              Right sender | props.provider.mustSetSender -> _from .~ Just sender
              _ ->
                -- NOTE: submit is disabled if sender is needed,
                -- so it's safe to ignore other cases in this case match.
                id
        mbTxHash <- lift $ runWeb3 props.provider.provider $ do
          SimpleStorage.setCount txOpts { _count : count }
        T.modifyState _
          { count = emptyInputVal :: InputVal Count
          , status = Just case mbTxHash of
              Left err -> FailedToCreateTransaction err 
              Right txHash -> TransactionWasCreated txHash
          }

    performAction (UpdateAddress input) _ _ = do
      let
        val = case input of
          "" -> Left "Input is required"
          i -> note "Please enter a valid ethereum address" $ mkAddress =<< mkHexString i
      void $ T.modifyState _{userAddress = {val, input}}

    performAction (UpdateCount input) _ _ = do
      let
        val = case input of
          "" -> Left "Input is required"
          i -> note "Please enter a valid uint256." $ uIntNFromBigNumber =<< parseBigNumber decimal i
      void $ T.modifyState _{count = {val, input}}

countFormClass :: R.ReactClass CountFormProps
countFormClass =
    let {spec} = T.createReactSpec countFormSpec (const $ pure initialCountFormState)
    in R.createClass (spec { componentWillMount = componentWillMount })
  where
  componentWillMount :: forall eff. R.ComponentWillMount CountFormProps CountFormState (eth :: ETH | eff)
  componentWillMount this = void $ launchAff $ do
    props <- liftEff $ R.getProps this
    let p = props.provider.provider
    when props.provider.mustSetSender
      $ void $ forkAff $ completeAddressField this p
    void $ forkAff $ do
      readCountAndUpdate this p
      monitorCount this p

type PropsAndStateRW r =
  ( props :: R.ReactProps
  , state :: R.ReactState R.ReadWrite
  | r)

completeAddressField
  :: forall eff
  . R.ReactThis CountFormProps CountFormState
  -> Provider
  -> Aff (PropsAndStateRW ( eth :: ETH | eff)) Unit
completeAddressField this p = do
  accounts <- map hush $ runWeb3 p $ eth_getAccounts
  for_ (accounts >>= flip index 0) \addr -> do
    liftEff $ R.transformState this _{userAddress = {val: Right addr, input: show addr}}

monitorCount
  :: forall eff
   . R.ReactThis CountFormProps CountFormState
  -> Provider
  -> Aff (PropsAndStateRW ( eth :: ETH | eff)) Unit
monitorCount this p = do
  err <- countEventFilter this p
  -- traceAnyA err
  -- NOTE: `err` is ignored as if we will try to read count with pull
  --  aproach here, which has error handling
  countPullLoop this p

readCountAndUpdate
  :: forall eff
   . R.ReactThis CountFormProps CountFormState
  -> Provider
  -> Aff (PropsAndStateRW ( eth :: ETH | eff)) Unit
readCountAndUpdate this p = do
  props <- liftEff $ R.getProps this
  let txOpts = defaultTransactionOptions # _to .~ Just props.contractAddress
  c <- runWeb3 p $ SimpleStorage.count txOpts Latest
  case c of
    Left err ->
      liftEff $ R.transformState this _{currentCount = Just $ Left $ FailedToFetchCountWithWeb3Error err}
    Right (Left err) ->
      liftEff $ R.transformState this _{currentCount = Just $ Left $ FailedToFetchCountWithCallError err}
    Right (Right count) ->
      liftEff $ R.transformState this \s -> 
        -- NOTE: besides updating `currentCount` here we also try to detect if the value
        -- is new, so `status` is updated in same way as it would in `countEventFilter`
        let nextCount = Just $ Right count
        in case s.status of
          Just (TransactionWasCreated _) -> case s.currentCount of
            Just (Right count') | count == count' -> s
            _ -> s{currentCount = nextCount, status = Just NewValueHasArrived}
          _ -> s{currentCount = nextCount}

countPullLoop
  :: forall eff void
   . R.ReactThis CountFormProps CountFormState
  -> Provider
  -> Aff (PropsAndStateRW ( eth :: ETH | eff)) void
countPullLoop this p = do
  readCountAndUpdate this p
  delay (Milliseconds 1000.0)
  countPullLoop this p

countEventFilter
  :: forall eff
   . R.ReactThis CountFormProps CountFormState
  -> Provider
  -> Aff (PropsAndStateRW ( eth :: ETH | eff)) (Either Error Web3Error)
countEventFilter this p = do
  props <- liftEff $ R.getProps this
  fiber <- forkWeb3 p $ do
    let fltr = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) props.contractAddress
    event fltr $ \(SimpleStorage.CountSet cs) -> do
      _ <- liftEff $ R.transformState this _{currentCount = Just $ Right cs._count, status = Just NewValueHasArrived}
      -- TODO: ideally we should termiante when component is unmounted
      pure ContinueEvent
  res <- try (joinFiber fiber)
  case res of
    Left err -> do
      -- # Error from aff
      -- there is an issue in ps-web3 when sendAsync is called it might fail but that's not handled
      -- so it propagates up to this point. ideally it should not happen and should be decoded into 
      -- Web3Error so `try` is not used to catch errors from `sendAsync`
      pure $ Left err
    Right (Left err) ->
      -- # Web3Error
      -- here are properly handled Web3Errors
      pure $ Right err
    Right (Right _) ->
      -- TODO: ideally we should stop listening when component is unmounted so this shuold be removed
      unsafeCrashWith "imposible, filter should never finishe, as event handler returns `ContinueEvent` all the time"
