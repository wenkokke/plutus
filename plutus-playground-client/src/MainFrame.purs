module MainFrame
  ( mainFrame
  , eval
  , initialState
  , mkInitialValue
  ) where

import Types
import Ace.Halogen.Component (AceMessage(TextChanged))
import Ace.Types (Annotation)
import Action (simulationPane)
import AjaxUtils (ajaxErrorPane)
import Analytics (Event, defaultEvent, trackEvent)
import Bootstrap (active, alert, alertPrimary, btn, btnGroup, btnSmall, colSm5, colSm6, colXs12, container, container_, empty, floatRight, hidden, justifyContentBetween, navItem_, navLink, navTabs_, noGutters, row)
import Chain (evaluationPane)
import Control.Bind (bindFlipped)
import Control.Comonad (extract)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.State (class MonadState, evalState)
import Control.Monad.Trans.Class (lift)
import Cursor (_current)
import Cursor as Cursor
import DOM.HTML.Indexed (Interactive)
import Data.Array (catMaybes, (..))
import Data.Array (concat, deleteAt, fromFoldable, snoc) as Array
import Data.Array.Extra (move) as Array
import Data.Either (Either(..), note)
import Data.Generic.Rep.Eq (genericEq)
import Data.Lens (_1, _2, _Just, _Right, assign, modifying, over, set, traversed, use, view)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (maximumOf, preview)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import Data.Newtype (unwrap)
import Data.RawJson (JsonEither(..), JsonTuple(..))
import Data.String as String
import Data.Traversable (foldl)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Editor (demoScriptsPane, editorPane)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Generic (decodeJSON)
import Gist (gistFileContent, gistId)
import Gists (gistControls)
import Gists as Gists
import Halogen (Component, action)
import Halogen as H
import Halogen.Component (ParentHTML)
import Halogen.HTML (class IsProp, ClassName(ClassName), ElemName(..), HTML, IProp, Node, PropName(..), a, code_, div, div_, element, h1, prop, strong_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, href, id_)
import Halogen.Query (HalogenM)
import Halogen.VDom.DOM.Prop (propFromString)
import Icons (Icon(..), icon)
import Language.Haskell.Interpreter (CompilationError(CompilationError, RawError), InterpreterError(CompilationErrors, TimeoutError), _InterpreterResult)
import Language.PlutusTx.AssocMap (Map(..))
import Language.PlutusTx.AssocMap as AssocMap
import Ledger.Ada (Ada(..))
import Ledger.Crypto (PubKey(..))
import Ledger.Interval (Interval(..))
import Ledger.Slot (Slot(..))
import Ledger.Tx (AddressOf(..), Tx(..), TxOutOf(..), TxOutType(..))
import Ledger.TxId (TxIdOf(..))
import Ledger.Value (CurrencySymbol(CurrencySymbol), TokenName(TokenName), Value(Value))
import MonadApp (class MonadApp, editorGetContents, editorGotoLine, editorSetAnnotations, editorSetContents, getGistByGistId, getOauthStatus, patchGistByGistId, postContract, postEvaluation, postGist, preventDefault, readFileFromDragEvent, runHalogenApp, saveBuffer, setDataTransferData, setDropEffect)
import Network.RemoteData (RemoteData(NotAsked, Loading, Failure, Success), _Success, isSuccess)
import Playground.API (KnownCurrency(..), SimulatorWallet(SimulatorWallet), _CompilationResult, _FunctionSchema)
import Playground.Gists (mkNewGist, playgroundGistFile, simulationGistFile)
import Playground.Server (SPParams_)
import Prelude (type (~>), Unit, Void, bind, const, discard, flip, join, map, pure, show, unit, unless, when, ($), (&&), (+), (-), (<$>), (<*>), (<<<), (<>), (==), (>>=))
import Prim.TypeError (class Warn, Text)
import Servant.PureScript.Settings (SPSettings_)
import StaticData as StaticData
import Wallet.Emulator.Types (Wallet(Wallet))
import Web.HTML.Event.DataTransfer as DataTransfer

aChain :: Blockchain
aChain = [ [ aSlot ] ]

aSlot :: JsonTuple (TxIdOf String) Tx
aSlot =
  JsonTuple
    ( Tuple (TxIdOf { getTxId: "9f189b187d18630918a90018c4185818ec18330018bd18df1826189d0b183218ed18a00c1846187d187d18e2183f14187718e30b185918e51825ff" })
        ( Tx
            { txFee: (Ada { getAda: 0 })
            , txForge: (Value { getValue: (Map [ (JsonTuple (Tuple (CurrencySymbol { unCurrencySymbol: "" }) (Map [ (JsonTuple (Tuple (TokenName { unTokenName: "" }) 20)) ]))), (JsonTuple (Tuple (CurrencySymbol { unCurrencySymbol: "b0b0" }) (Map [ (JsonTuple (Tuple (TokenName { unTokenName: "EURToken" }) 20)), (JsonTuple (Tuple (TokenName { unTokenName: "USDToken" }) 20)) ]))) ]) })
            , txInputs: []
            , txOutputs:
              [ ( TxOutOf
                    { txOutAddress: (AddressOf { getAddress: "9f1827182118f6185718e918ed189118d218fc182a1828182f187f18f518ed188118ae184818f4188b185106181a0a18a218fa18a418b2185b185f031822ff" })
                    , txOutType: (PayToPubKey (PubKey { getPubKey: "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c" }))
                    , txOutValue:
                      ( Value
                          { getValue:
                            ( Map
                                [ (JsonTuple (Tuple (CurrencySymbol { unCurrencySymbol: "" }) (Map [ (JsonTuple (Tuple (TokenName { unTokenName: "" }) 10)) ])))
                                , ( JsonTuple
                                      ( Tuple (CurrencySymbol { unCurrencySymbol: "b0b0" })
                                          ( Map
                                              [ (JsonTuple (Tuple (TokenName { unTokenName: "EURToken" }) 10))
                                              , (JsonTuple (Tuple (TokenName { unTokenName: "USDToken" }) 10))
                                              ]
                                          )
                                      )
                                  )
                                ]
                            )
                          }
                      )
                    }
                )
              , ( TxOutOf
                    { txOutAddress: (AddressOf { getAddress: "9f0318d20018a8181e18e018fe18ac18e818fb1884185e185e18c9185018a618f918ad18d81837091824184f187b1881131846185413189f184118a4ff" })
                    , txOutType: (PayToPubKey (PubKey { getPubKey: "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025" }))
                    , txOutValue: (Value { getValue: (Map [ (JsonTuple (Tuple (CurrencySymbol { unCurrencySymbol: "" }) (Map [ (JsonTuple (Tuple (TokenName { unTokenName: "" }) 10)) ]))), (JsonTuple (Tuple (CurrencySymbol { unCurrencySymbol: "b0b0" }) (Map [ (JsonTuple (Tuple (TokenName { unTokenName: "EURToken" }) 10)), (JsonTuple (Tuple (TokenName { unTokenName: "USDToken" }) 10)) ]))) ]) })
                    }
                )
              ]
            , txSignatures: (Map [])
            , txValidRange: (Interval { ivFrom: (Just (Slot { getSlot: 0 })), ivTo: Nothing })
            }
        )
    )

mkSimulatorWallet :: Array KnownCurrency -> Int -> SimulatorWallet
mkSimulatorWallet currencies walletId =
  SimulatorWallet
    { simulatorWalletWallet: Wallet { getWallet: walletId }
    , simulatorWalletBalance: mkInitialValue currencies 10
    }

mkInitialValue :: Array KnownCurrency -> Int -> Value
mkInitialValue currencies initialBalance = Value { getValue: value }
  where
  value =
    foldl
      (AssocMap.unionWith (AssocMap.unionWith (+)))
      (AssocMap.fromTuples [])
      $ Array.concat
      $ map
          ( \(KnownCurrency { hash, knownTokens }) ->
              map
                ( \(TokenName tokenId) ->
                    AssocMap.fromTuples
                      [ CurrencySymbol { unCurrencySymbol: hash }
                          /\ AssocMap.fromTuples [ TokenName tokenId /\ initialBalance ]
                      ]
                )
                $ Array.fromFoldable knownTokens
          )
          currencies

mkSimulation :: Array KnownCurrency -> Signatures -> Simulation
mkSimulation currencies signatures =
  Simulation
    { signatures
    , actions: []
    , wallets: mkSimulatorWallet currencies <$> 1 .. 2
    , currencies
    }

initialState :: State
initialState =
  State
    { currentView: Editor
    , compilationResult: NotAsked
    , simulations: Cursor.empty
    , actionDrag: Nothing
    , evaluationResult: NotAsked
    , authStatus: NotAsked
    , createGistResult: NotAsked
    , gistUrl: Nothing
    }

------------------------------------------------------------
mainFrame ::
  forall m.
  MonadAff m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  Component HTML Query Unit Void m
mainFrame =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval: evalWithAnalyticsTracking
    , receiver: const Nothing
    , initializer: Just $ H.action $ CheckAuthStatus
    , finalizer: Nothing
    }

evalWithAnalyticsTracking ::
  forall m.
  MonadEffect m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadAff m =>
  Query ~> HalogenM State Query ChildQuery ChildSlot Void m
evalWithAnalyticsTracking query = do
  liftEffect $ analyticsTracking query
  runHalogenApp $ eval query

analyticsTracking :: forall a. Query a -> Effect Unit
analyticsTracking query = do
  case toEvent query of
    Nothing -> pure unit
    Just event -> trackEvent event

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
toEvent :: forall a. Query a -> Maybe Event
toEvent (HandleEditorMessage _ _) = Nothing

toEvent (HandleDragEvent _ _) = Nothing

toEvent (HandleDropEvent _ _) = Just $ defaultEvent "DropScript"

toEvent (HandleBalancesChartMessage _ _) = Nothing

toEvent (CheckAuthStatus _) = Nothing

toEvent (PublishGist _) = Just $ (defaultEvent "Publish") { category = Just "Gist" }

toEvent (SetGistUrl _ _) = Nothing

toEvent (LoadGist _) = Just $ (defaultEvent "LoadGist") { category = Just "Gist" }

toEvent (ChangeView view _) = Just $ (defaultEvent "View") { label = Just $ show view }

toEvent (LoadScript script a) = Just $ (defaultEvent "LoadScript") { label = Just script }

toEvent (CompileProgram a) = Just $ defaultEvent "CompileProgram"

toEvent (ScrollTo _ _) = Nothing

toEvent (AddSimulationSlot _) = Just $ (defaultEvent "AddSimulationSlot") { category = Just "Simulation" }

toEvent (SetSimulationSlot _ _) = Just $ (defaultEvent "SetSimulationSlot") { category = Just "Simulation" }

toEvent (RemoveSimulationSlot _ _) = Just $ (defaultEvent "RemoveSimulationSlot") { category = Just "Simulation" }

toEvent (ModifyWallets AddWallet _) = Just $ (defaultEvent "AddWallet") { category = Just "Wallet" }

toEvent (ModifyWallets (RemoveWallet _) _) = Just $ (defaultEvent "RemoveWallet") { category = Just "Wallet" }

toEvent (ModifyWallets (ModifyBalance _ (SetBalance _ _ _)) _) = Just $ (defaultEvent "SetBalance") { category = Just "Wallet" }

toEvent (ModifyActions (AddAction _) _) = Just $ (defaultEvent "AddAction") { category = Just "Action" }

toEvent (ActionDragAndDrop _ eventType _ _) = Just $ (defaultEvent (show eventType)) { category = Just "Action" }

toEvent (ModifyActions (AddWaitAction _) _) = Just $ (defaultEvent "AddWaitAction") { category = Just "Action" }

toEvent (ModifyActions (RemoveAction _) _) = Just $ (defaultEvent "RemoveAction") { category = Just "Action" }

toEvent (ModifyActions (SetWaitTime _ _) _) = Just $ (defaultEvent "SetWaitTime") { category = Just "Action" }

toEvent (EvaluateActions _) = Just $ (defaultEvent "EvaluateActions") { category = Just "Action" }

toEvent (PopulateAction _ _ _) = Just $ (defaultEvent "PopulateAction") { category = Just "Action" }

eval ::
  forall m.
  MonadState State m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadApp m =>
  Query ~> m
eval (HandleEditorMessage (TextChanged text) next) = do
  saveBuffer text
  pure next

eval (ActionDragAndDrop index DragStart event next) = do
  setDataTransferData event textPlain (show index)
  assign _actionDrag (Just index)
  pure next

eval (ActionDragAndDrop _ DragEnd event next) = do
  assign _actionDrag Nothing
  pure next

eval (ActionDragAndDrop _ DragEnter event next) = do
  preventDefault event
  setDropEffect DataTransfer.Move event
  pure next

eval (ActionDragAndDrop _ DragOver event next) = do
  preventDefault event
  setDropEffect DataTransfer.Move event
  pure next

eval (ActionDragAndDrop _ DragLeave event next) = do
  pure next

eval (ActionDragAndDrop destination Drop event next) = do
  use _actionDrag
    >>= case _ of
        Just source -> modifying (_simulations <<< _current <<< _actions) (Array.move source destination)
        _ -> pure unit
  preventDefault event
  assign _actionDrag Nothing
  pure next

eval (HandleDragEvent event next) = do
  preventDefault event
  pure next

eval (HandleDropEvent event next) = do
  preventDefault event
  contents <- readFileFromDragEvent event
  editorSetContents contents (Just 1)
  saveBuffer contents
  pure next

-- We just ignore most Chartist events.
eval (HandleBalancesChartMessage _ next) = pure next

eval (CheckAuthStatus next) = do
  assign _authStatus Loading
  authResult <- getOauthStatus
  assign _authStatus authResult
  pure next

eval (PublishGist next) = do
  mContents <- editorGetContents
  simulations <- use _simulations
  case mkNewGist { source: mContents, simulations } of
    Nothing -> pure next
    Just newGist -> do
      mGist <- use _createGistResult
      assign _createGistResult Loading
      newResult <- case preview (_Success <<< gistId) mGist of
        Nothing -> postGist newGist
        Just gistId -> patchGistByGistId newGist gistId
      assign _createGistResult newResult
      case preview (_Success <<< gistId) newResult of
        Nothing -> pure unit
        Just gistId -> assign _gistUrl (Just (unwrap gistId))
      pure next

eval (SetGistUrl newGistUrl next) = do
  assign _gistUrl (Just newGistUrl)
  pure next

eval (LoadGist next) = do
  eGistId <- (bindFlipped Gists.parseGistUrl <<< note "Gist Url not set.") <$> use _gistUrl
  case eGistId of
    Left err -> pure unit
    Right gistId -> do
      assign _createGistResult Loading
      aGist <- getGistByGistId gistId
      assign _createGistResult aGist
      case aGist of
        Success gist -> do
          -- Load the source, if available.
          case preview (_Just <<< gistFileContent <<< _Just) (playgroundGistFile gist) of
            Nothing -> pure unit
            Just content -> do
              editorSetContents content (Just 1)
              saveBuffer content
              assign _simulations Cursor.empty
              assign _evaluationResult NotAsked
          -- Load the simulation, if available.
          case preview (_Just <<< gistFileContent <<< _Just) (simulationGistFile gist) of
            Nothing -> pure unit
            Just simulationString -> do
              case runExcept (decodeJSON simulationString) of
                Left err -> pure unit
                Right simulations -> do
                  assign _simulations simulations
                  assign _evaluationResult NotAsked
        _ -> pure unit
  pure next

eval (ChangeView view next) = do
  assign _currentView view
  pure next

eval (LoadScript key next) = do
  case Map.lookup key StaticData.demoFiles of
    Nothing -> pure next
    Just contents -> do
      editorSetContents contents (Just 1)
      saveBuffer contents
      assign _currentView Editor
      assign _compilationResult NotAsked
      assign _evaluationResult NotAsked
      assign _simulations Cursor.empty
      pure next

eval (CompileProgram next) = do
  mContents <- editorGetContents
  case mContents of
    Nothing -> pure next
    Just contents -> do
      assign _compilationResult Loading
      result <- postContract contents
      assign _compilationResult result
      -- If we got a successful result, switch tab.
      case result of
        Success (JsonEither (Left _)) -> pure unit
        _ -> replaceViewOnSuccess result Editor Simulations
      -- Update the error display.
      editorSetAnnotations
        $ case result of
            Success (JsonEither (Left errors)) -> toAnnotations errors
            _ -> []
      -- If we have a result with new signatures, we can only hold
      -- onto the old actions if the signatures still match. Any
      -- change means we'll have to clear out the existing simulation.
      -- Same thing for currencies.
      -- Potentially we could be smarter about this. But for now,
      -- let's at least be correct.
      case preview (_Success <<< _Newtype <<< _Right <<< _InterpreterResult <<< _result <<< _CompilationResult) result of
        Just { functionSchema: newSignatures, knownCurrencies: newCurrencies } -> do
          oldSimulation <- peruse (_simulations <<< _current <<< _Newtype)
          unless
            ( ((_.signatures <$> oldSimulation) `genericEq` Just newSignatures)
                && ((_.currencies <$> oldSimulation) `genericEq` Just newCurrencies)
            )
            (assign _simulations $ Cursor.singleton $ mkSimulation newCurrencies newSignatures)
        _ -> pure unit
      pure next

eval (ScrollTo { row, column } next) = do
  editorGotoLine row (Just column)
  pure next

eval (ModifyActions actionEvent next) = do
  modifying (_simulations <<< _current <<< _actions) (evalActionEvent actionEvent)
  pure next

eval (EvaluateActions next) = do
  _ <-
    runMaybeT
      $ do
          evaluation <-
            MaybeT do
              contents <- editorGetContents
              simulation <- peruse (_simulations <<< _current)
              pure $ join $ toEvaluation <$> contents <*> simulation
          assign _evaluationResult Loading
          result <- lift $ postEvaluation evaluation
          assign _evaluationResult result
          replaceViewOnSuccess result Simulations Transactions
          pure unit
  pure next

eval (AddSimulationSlot next) = do
  knownCurrencies <- getKnownCurrencies
  mSignatures <- peruse (_compilationResult <<< _Success <<< _Newtype <<< _Right <<< _InterpreterResult <<< _result <<< _CompilationResult <<< _functionSchema)
  case mSignatures of
    Just signatures -> modifying _simulations (flip Cursor.snoc (mkSimulation knownCurrencies signatures))
    Nothing -> pure unit
  pure next

eval (SetSimulationSlot index next) = do
  modifying _simulations (Cursor.setIndex index)
  pure next

eval (RemoveSimulationSlot index next) = do
  modifying _simulations (Cursor.deleteAt index)
  pure next

eval (ModifyWallets action next) = do
  knownCurrencies <- getKnownCurrencies
  modifying (_simulations <<< _current <<< _wallets) (evalWalletEvent (mkSimulatorWallet knownCurrencies) action)
  pure next

eval (PopulateAction n l event) = do
  knownCurrencies <- getKnownCurrencies
  let
    initialValue = mkInitialValue knownCurrencies 0
  modifying
    ( _simulations
        <<< _current
        <<< _actions
        <<< ix n
        <<< _Action
        <<< _functionSchema
        <<< _FunctionSchema
        <<< _argumentSchema
        <<< ix l
    )
    (evalForm initialValue event)
  pure $ extract event

getKnownCurrencies :: forall m. MonadState State m => m (Array KnownCurrency)
getKnownCurrencies = do
  knownCurrencies <- peruse (_compilationResult <<< _Success <<< _Newtype <<< _Right <<< _InterpreterResult <<< _result <<< _knownCurrencies)
  pure $ fromMaybe [] knownCurrencies

evalWalletEvent :: (Int -> SimulatorWallet) -> WalletEvent -> Array SimulatorWallet -> Array SimulatorWallet
evalWalletEvent mkWallet AddWallet wallets =
  let
    maxWalletId = fromMaybe 0 $ maximumOf (traversed <<< _simulatorWalletWallet <<< _walletId) wallets

    newWallet = mkWallet (maxWalletId + 1)
  in
    Array.snoc wallets newWallet

evalWalletEvent _ (RemoveWallet index) wallets = fromMaybe wallets $ Array.deleteAt index wallets

evalWalletEvent _ (ModifyBalance walletIndex action) wallets =
  over
    (ix walletIndex <<< _simulatorWalletBalance)
    (evalValueEvent action)
    wallets

evalValueEvent :: ValueEvent -> Value -> Value
evalValueEvent (SetBalance currencySymbol tokenName amount) = set (_value <<< ix currencySymbol <<< ix tokenName) amount

evalActionEvent :: ActionEvent -> Array Action -> Array Action
evalActionEvent (AddAction action) = flip Array.snoc action

evalActionEvent (AddWaitAction blocks) = flip Array.snoc (Wait { blocks })

evalActionEvent (RemoveAction index) = fromMaybe <*> Array.deleteAt index

evalActionEvent (SetWaitTime index time) = set (ix index <<< _Wait <<< _blocks) time

evalForm ::
  forall a.
  Warn (Text "I wonder if this code would be simpler if we just abandoned FormArgument and used the Functor version everywhere.") =>
  Value ->
  FormEvent a ->
  FormArgument ->
  FormArgument
evalForm initialValue = rec
  where
  rec (SetIntField n next) (FormInt _) = FormInt n

  rec (SetIntField _ _) arg = arg

  rec (SetBoolField n next) (FormBool _) = FormBool n

  rec (SetBoolField _ _) arg = arg

  rec (SetStringField s next) (FormString _) = FormString (Just s)

  rec (SetStringField _ _) arg = arg

  rec (SetHexField s next) (FormHex _) = FormHex (Just s)

  rec (SetHexField _ _) arg = arg

  rec (SetRadioField s next) (FormRadio options _) = FormRadio options (Just s)

  rec (SetRadioField _ _) arg = arg

  rec (SetValueField valueEvent _) (FormValue value) = FormValue $ evalValueEvent valueEvent value

  rec (SetValueField _ _) arg = arg

  rec (SetSubField 1 subEvent) (FormTuple fields) = FormTuple $ over (_Newtype <<< _1) (rec subEvent) fields

  rec (SetSubField 2 subEvent) (FormTuple fields) = FormTuple $ over (_Newtype <<< _2) (rec subEvent) fields

  rec (SetSubField _ subEvent) arg@(FormTuple _) = arg

  rec (SetSubField _ subEvent) arg@(FormString _) = arg

  rec (SetSubField _ subEvent) arg@(FormInt _) = arg

  rec (SetSubField _ subEvent) arg@(FormBool _) = arg

  rec (SetSubField _ subEvent) arg@(FormHex _) = arg

  rec (SetSubField _ subEvent) arg@(FormRadio _ _) = arg

  rec (SetSubField _ subEvent) arg@(FormValue _) = arg

  rec (AddSubField _) (FormArray schema fields) =  -- As the code stands, this is the only guarantee we get that every -- value in the array will conform to the schema: the fact that we -- create the 'empty' version from the same schema template.
    --
    -- Is more type safety than that possible? Probably.
    -- Is it worth the research effort? Perhaps. :thinking_face:
    FormArray schema $ Array.snoc fields (toArgument initialValue schema)

  rec (AddSubField _) arg = arg

  rec (SetSubField 0 subEvent) (FormMaybe schema field) = FormMaybe schema $ over _Just (rec subEvent) field

  rec (SetSubField _ subEvent) arg@(FormMaybe schema field) = arg

  rec (SetSubField n subEvent) (FormArray schema fields) = FormArray schema $ over (ix n) (rec subEvent) fields

  rec (SetSubField n subEvent) s@(FormObject fields) = FormObject $ over (ix n <<< _Newtype <<< _2) (rec subEvent) fields

  rec (SetSubField n subEvent) arg@(FormUnsupported _) = arg

  rec (RemoveSubField n subEvent) arg@(FormArray schema fields) = (FormArray schema (fromMaybe fields (Array.deleteAt n fields)))

  rec (RemoveSubField n subEvent) arg = arg

replaceViewOnSuccess :: forall m e a. MonadState State m => RemoteData e a -> View -> View -> m Unit
replaceViewOnSuccess result source target = do
  currentView <- use _currentView
  when (isSuccess result && currentView == source)
    (assign _currentView target)

------------------------------------------------------------
toAnnotations :: InterpreterError -> Array Annotation
toAnnotations (TimeoutError _) = []

toAnnotations (CompilationErrors errors) = catMaybes (toAnnotation <$> errors)

toAnnotation :: CompilationError -> Maybe Annotation
toAnnotation (RawError _) = Nothing

toAnnotation (CompilationError { row, column, text }) =
  Just
    { type: "error"
    , row: row - 1
    , column
    , text: String.joinWith "\n" text
    }

render ::
  forall m.
  MonadAff m =>
  State -> ParentHTML Query ChildQuery ChildSlot m
render state@(State { currentView }) =
  div_
    [ bannerMessage
    , div
        [ class_ $ ClassName "main-frame" ]
        [ container_
            [ mainHeader
            , div [ classes [ row, noGutters, justifyContentBetween ] ]
                [ div [ classes [ colXs12, colSm6 ] ] [ mainTabBar currentView ]
                , div [ classes [ colXs12, colSm5 ] ] [ gistControls (unwrap state) ]
                ]
            ]
        , viewContainer currentView Editor
            $ [ demoScriptsPane
              , div_ [ code_ [ text $ show aChain ] ]
              , slotVisualisation aSlot
              , editorPane defaultContents (map unwrap (view _compilationResult state))
              , case view _compilationResult state of
                  Failure error -> ajaxErrorPane error
                  _ -> empty
              ]
        , viewContainer currentView Simulations
            $ let
                knownCurrencies = evalState getKnownCurrencies state

                initialValue = mkInitialValue knownCurrencies 0
              in
                [ simulationPane
                    initialValue
                    (view _actionDrag state)
                    (view _simulations state)
                    (view _evaluationResult state)
                , case (view _evaluationResult state) of
                    Failure error -> ajaxErrorPane error
                    _ -> empty
                ]
        , viewContainer currentView Transactions
            $ case view _evaluationResult state of
                Success evaluation -> [ evaluationPane evaluation ]
                Failure error ->
                  [ text "Your simulation has errors. Click the "
                  , strong_ [ text "Simulation" ]
                  , text " tab above to fix them and recompile."
                  ]
                Loading -> [ icon Spinner ]
                NotAsked ->
                  [ text "Click the "
                  , strong_ [ text "Simulation" ]
                  , text " tab above and evaluate a simulation to see some results."
                  ]
        ]
    ]
  where
  defaultContents = Map.lookup "Vesting" StaticData.demoFiles

bannerMessage :: forall p i. HTML p i
bannerMessage =
  div
    [ id_ "banner-message"
    , classes [ alert, alertPrimary ]
    ]
    [ text "Plutus Beta - Updated 11th April 2019 - See the "
    , a
        [ href ("https://github.com/input-output-hk/plutus/blob/master/CHANGELOG.md") ]
        [ text "CHANGELOG" ]
    ]

viewContainer :: forall p i. View -> View -> Array (HTML p i) -> HTML p i
viewContainer currentView targetView =
  if currentView == targetView then
    div [ classes [ container ] ]
  else
    div [ classes [ container, hidden ] ]

mainHeader :: forall p. HTML p (Query Unit)
mainHeader =
  div_
    [ div [ classes [ btnGroup, floatRight ] ]
        (makeLink <$> links)
    , h1
        [ class_ $ ClassName "main-title" ]
        [ text "Plutus Playground" ]
    ]
  where
  links =
    [ Tuple "Getting Started" "https://testnet.iohkdev.io/plutus/get-started/writing-contracts-in-plutus/"
    , Tuple "Tutorial" "./tutorial"
    , Tuple "API" "./haddock"
    , Tuple "Privacy" "https://static.iohk.io/docs/data-protection/iohk-data-protection-gdpr-policy.pdf"
    ]

  makeLink (Tuple name link) =
    a
      [ classes [ btn, btnSmall ]
      , href link
      ]
      [ text name ]

mainTabBar :: forall p. View -> HTML p (Query Unit)
mainTabBar activeView = navTabs_ (mkTab <$> tabs)
  where
  tabs =
    [ Editor /\ "Editor"
    , Simulations /\ "Simulation"
    , Transactions /\ "Transactions"
    ]

  mkTab :: Tuple View String -> HTML p (Query Unit)
  mkTab (link /\ title) =
    navItem_
      [ a
          [ id_ $ "tab-" <> String.toLower (show link)
          , classes $ [ navLink ] <> activeClass
          , onClick $ const $ Just $ action $ ChangeView link
          ]
          [ text title ]
      ]
    where
    activeClass =
      if link == activeView then
        [ active ]
      else
        []

slotVisualisation :: forall p i. JsonTuple (TxIdOf String) Tx -> HTML p i
slotVisualisation slot =
  svg
    -- [ viewBox $ Rect { x: 0, y: 0, height: 400, width: 800 }
    -- , xmlns "http://www.w3.org/2000/svg"
    -- ]
    [ xmlns "http://www.w3.org/2000/svg"
    ]
    []

data Rect
  = Rect
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }

instance isPropRect :: IsProp Rect where
  toPropValue (Rect rect) =
    spy "PROP"
      $ propFromString
      $ show rect.x
      <> " "
      <> show rect.y
      <> " "
      <> show rect.height
      <> " "
      <> show rect.width

type SVGNode
  = Interactive
      ( viewBox :: Rect
      , xmlns :: String
      )

svg :: forall p i. Node SVGNode p i
svg = element (ElemName "svg")

viewBox :: forall r i. Rect -> IProp ( viewBox :: Rect | r ) i
viewBox = prop (PropName "viewBox")

xmlns :: forall r i. String -> IProp ( xmlns :: String | r ) i
xmlns = prop (PropName "xmlns")
