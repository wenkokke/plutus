module Simulation where

import Marlowe.Semantics

import API (RunResult(RunResult))
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Halogen.Component (Autocomplete(Live), aceComponent)
import Ace.Types (Editor)
import Bootstrap (btn, btnInfo, btnPrimary, btnSmall, cardBody_, card, card_, col6, col_, row_, empty, listGroupItem_, listGroup_)
import Control.Alternative (map, (<|>))
import Data.Array (catMaybes, foldMap, mapWithIndex)
import Data.Array as Array
import Data.BigInteger (BigInteger, fromString, fromInt)
import Data.Either (Either(..))
import Data.Eq ((==), (/=))
import Data.Foldable (intercalate)
import Data.HeytingAlgebra ((&&))
import Data.Lens (to, view, (^.))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (HTML, action)
import Halogen.Component (ParentHTML)
import Halogen.HTML (ClassName(ClassName), b_, br_, button, code_, col, colgroup, div, div_, h2, h3_, input, li_, pre_, slot', span, strong_, table_, tbody_, td, td_, text, th, th_, thead_, tr, ul_)
import Halogen.HTML.Events (input_, onClick, onDragOver, onDrop, onValueChange)
import Halogen.HTML.Events as Events
import Halogen.HTML.Properties (InputType(InputNumber), class_, classes, enabled, placeholder, type_, value)
import Halogen.Query as HQ
import LocalStorage as LocalStorage
import Prelude (class Show, Unit, bind, const, discard, flip, identity, pure, show, unit, void, ($), (<$>), (<<<), (<>), (>))
import StaticData as StaticData
import Types (ActionInput(..), ChildQuery, ChildSlot, FrontendState, MarloweEditorSlot(MarloweEditorSlot), MarloweError(MarloweError), MarloweState, Query(..), _Head, _contract, _marloweCompileResult, _marloweState, _moneyInContract, _oldContract, _pendingInputs, _possibleActions, _slot, _state, _transactionError, cpMarloweEditor)

paneHeader :: forall p. String -> HTML p Query
paneHeader s = h2 [class_ $ ClassName "pane-header"] [text s]

isContractValid :: FrontendState -> Boolean
isContractValid state = view (_marloweState <<< _Head <<< _contract) state /= Nothing

simulationPane ::
  forall m.
  MonadAff m =>
  FrontendState ->
  ParentHTML Query ChildQuery ChildSlot m
simulationPane state =
  div_
    ( Array.concat
      [ [ row_
            [ inputComposerPane state
            , transactionComposerPane state
            ]
        , stateTitle state
        , row_ [statePane state]
        ]
      , state ^. (_marloweState <<< _Head <<< _transactionError <<< to transactionErrors)
      , contractParsingError (isContractValid state)
      , [ div
            [ classes
                [ ClassName "demos"
                , ClassName "d-flex"
                , ClassName "flex-row"
                , ClassName "align-items-center"
                , ClassName "justify-content-between"
                , ClassName "mt-5"
                , ClassName "mb-3"
                ]
            ] [paneHeader "Marlowe Contract", codeToBlocklyButton state, demoScriptsPane]
        , div
            [ onDragOver $ Just <<< action <<< MarloweHandleDragEvent
            , onDrop $ Just <<< action <<< MarloweHandleDropEvent
            ]
            [ slot' cpMarloweEditor MarloweEditorSlot (aceComponent initEditor (Just Live)) unit (Events.input MarloweHandleEditorMessage)
            ]
        , br_
        , errorList
        ]
      ]
    )
  where
  errorList = case view _marloweCompileResult state of
    Left errors -> listGroup_ (listGroupItem_ <<< pure <<< compilationErrorPane <$> errors)
    _ -> empty

loadBuffer :: Effect (Maybe String)
loadBuffer = LocalStorage.getItem StaticData.marloweBufferLocalStorageKey

initEditor ::
  forall m.
  MonadAff m =>
  Editor ->
  m Unit
initEditor editor =
  liftEffect
    $ do
        savedContents <- liftEffect loadBuffer
        let
          defaultContents = Map.lookup "Deposit Incentive" StaticData.marloweContracts
        let
          contents = fromMaybe "" (savedContents <|> defaultContents)
        void $ Editor.setValue contents (Just 1) editor
        Editor.setTheme "ace/theme/monokai" editor
        session <- Editor.getSession editor
        Session.setMode "ace/mode/haskell" session

demoScriptsPane :: forall p. HTML p Query
demoScriptsPane =
  div_
    ( Array.cons
      ( strong_
        [ text "Demos: "
        ]
      ) (demoScriptButton <$> Array.fromFoldable (Map.keys StaticData.marloweContracts))
    )

demoScriptButton :: forall p. String -> HTML p Query
demoScriptButton key =
  button
    [ classes [btn, btnInfo, btnSmall]
    , onClick $ input_ $ LoadMarloweScript key
    ] [text key]

codeToBlocklyButton :: forall p. FrontendState -> HTML p Query
codeToBlocklyButton state =
  button
    [ classes [btn, btnInfo, btnSmall]
    , onClick $ input_ $ SetBlocklyCode
    , enabled (isContractValid state)
    ] [text "Code to Blockly"]

compilationResultPane :: forall p. RunResult -> HTML p Query
compilationResultPane (RunResult stdout) = div_ [code_ [pre_ [text stdout]]]

compilationErrorPane :: forall p. MarloweError -> HTML p Query
compilationErrorPane (MarloweError error) = div_ [text error]

inputComposerPane :: forall p. FrontendState -> HTML p Query
inputComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Input Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ card_
            [ cardBody_ (inputComposer isEnabled (view (_marloweState <<< _Head <<< _possibleActions) state))
            ]
        ]
    ]
  where
  isEnabled = isContractValid state

onEmpty :: forall a. Array a -> Array a -> Array a
onEmpty alt [] = alt

onEmpty _ arr = arr

inputComposer :: forall p. Boolean -> Map PubKey (Array ActionInput) -> Array (HTML p Query)
inputComposer isEnabled actionInputs =
  onEmpty 
    [text "No valid inputs can be added to the transaction"]
    $ actionsForPeople actionInputs
  where
  kvs :: forall k v. Map k v -> Array (Tuple k v)
  kvs = Map.toUnfoldable

  actionsForPeople :: forall q. Map PubKey (Array ActionInput) -> Array (HTML q Query)
  actionsForPeople m = foldMap (\(Tuple k v) -> inputComposerPerson isEnabled k v) (kvs m)

inputComposerPerson ::
  forall p.
  Boolean ->
  PubKey ->
  Array ActionInput ->
  Array (HTML p Query)
inputComposerPerson isEnabled person actionInputs =
      [ h3_
          [ text ("Person " <> show person)
          ]
      ] <> catMaybes (mapWithIndex inputForAction actionInputs)
  where
    inputForAction :: Int -> ActionInput -> Maybe (HTML p Query)
    inputForAction _ (DepositInput false _ _ _) = Nothing
    inputForAction _ (ChoiceInput false _ _ _) = Nothing
    inputForAction _ (NotifyInput false _) = Nothing
    inputForAction index (DepositInput true accountId party value) = Just $ inputDeposit isEnabled person index accountId party value
    inputForAction index (ChoiceInput true choiceId bounds chosenNum) = Just $ inputChoice isEnabled person index choiceId chosenNum bounds
    inputForAction index (NotifyInput true observation) = Just $ inputNotify isEnabled person index observation

inputDeposit ::
  forall p.
  Boolean ->
  PubKey ->
  Int ->
  AccountId ->
  Party ->
  BigInteger ->
  HTML p Query
inputDeposit isEnabled person index accountId party value =
  let money = wrap value in
  flexRow_ $
    [ button
        [ class_ $ ClassName "composer-add-button"
        , enabled isEnabled
        , onClick $ input_
            $ AddInput person index $ IDeposit accountId party money
        ]
        [ text "+"
        ]
    ] <> (renderDeposit accountId party value)

renderDeposit :: forall p. AccountId -> Party -> BigInteger -> Array (HTML p Query)
renderDeposit (AccountId { accountOwner, accountNumber }) party money =
    [ spanText "Deposit "
    , b_ [spanText (show money)]
    , spanText " ADA into Account "
    , b_ [spanText (accountOwner <> " (" <> show accountNumber <> ")")]
    , spanText " as "
    , b_ [spanText party]
    ]

inputChoice :: forall p. Boolean -> PubKey -> Int -> ChoiceId -> ChosenNum -> Array Bound -> HTML p Query
inputChoice isEnabled person index choiceId@(ChoiceId { choiceNumber, choiceOwner}) chosenNum bounds =
  let validBounds = anyWithin chosenNum bounds
      errorRow = if validBounds then [] else [ text boundsError ]
  in
  flexRow_
    ([ button
        [ class_ $ ClassName "composer-add-button"
        , enabled (isEnabled && validBounds)
        , onClick $ input_
            $ AddInput person index $ IChoice (wrap { choiceNumber, choiceOwner }) chosenNum
        ]
        [ text "+"
        ]
    , spanText "Choice "
    , b_ [spanText (show choiceNumber)]
    , spanText ": Choose value "
    , marloweActionInput isEnabled (SetChoice choiceId) chosenNum
    ] <> errorRow)
    where
    boundsError = "Choice must be between " <> intercalate " or " (map boundError bounds)
    boundError bound = show (unwrap bound).from <> " and " <> show (unwrap bound).to


inputNotify ::
  forall p.
  Boolean ->
  PubKey ->
  Int ->
  Observation ->
  HTML p Query
inputNotify isEnabled person index observation =
  flexRow_
    [ button
        [ class_ $ ClassName "composer-add-button"
        , enabled isEnabled
        , onClick $ input_
            $ AddInput person index $ INotify
        ]
        [ text "+"
        ]
    , text $ "Notify " <> show observation
    ]

marloweActionInput :: forall p a. Show a => Boolean -> (BigInteger -> Unit -> Query Unit) -> a -> HTML p Query
marloweActionInput isEnabled f current =
  input
    [ type_ InputNumber
    , enabled isEnabled
    , placeholder "BigInteger"
    , class_ $ ClassName "action-input"
    , value $ show current
    , onValueChange
        $ ( \x ->
            Just $ HQ.action
              $ f
                  ( case fromString x of
                    Just y -> y
                    Nothing -> fromInt 0
                  )
          )
    ]

flexRow_ ::
  forall p.
  Array (HTML p Query) ->
  HTML p Query
flexRow_ html = div [classes [ClassName "d-flex", ClassName "flex-row"]] html

spanText :: forall p. String -> HTML p Query
spanText s = span [class_ $ ClassName "pr-1"] [text s]

transactionComposerPane ::
  forall p.
  FrontendState ->
  HTML p Query
transactionComposerPane state =
  div
    [ classes
        [ col6
        , ClassName "input-composer"
        ]
    ]
    [ paneHeader "Transaction Composer"
    , div
        [ class_ $ ClassName "wallet"
        ]
        [ div
            [ classes
                ( ( if view (_marloweState <<< _Head <<< _transactionError <<< to isJust) state
                  then (flip Array.snoc) (ClassName "invalid-transaction")
                  else identity
                ) [card]
                )
            ]
            [ cardBody_ $ transactionInputs (view (_marloweState <<< _Head) state)
                -- <> ( signatures (view (_marloweState <<< _Head <<< _transaction <<< _signatures) state) (isContractValid state) (view (_marloweState <<< _Head <<< _transaction <<< _outcomes) state)
                --   )
                <> transactionButtons state
            ]
        ]
    ]

transactionButtons :: FrontendState -> forall p. Array (HTML p Query)
transactionButtons state =
  [ div
      [ classes
          [ ClassName "d-flex"
          , ClassName "flex-row"
          , ClassName "align-items-center"
          , ClassName "justify-content-start"
          , ClassName "transaction-btn-row"
          ]
      ]
      [ button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< HQ.action <<< const ApplyTransaction
          , enabled $ isContractValid state
          ] [text "Apply Transaction"]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , onClick $ Just <<< HQ.action <<< const NextSlot
          , enabled (isContractValid state)
          ] [text "Next Block"]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (view _oldContract state /= Nothing)
          , onClick $ Just <<< HQ.action <<< const ResetSimulator
          ] [text "Reset"]
      , button
          [ classes
              [ btn
              , btnPrimary
              , ClassName "transaction-btn"
              ]
          , enabled (hasHistory state)
          , onClick $ Just <<< HQ.action <<< const Undo
          ] [text "Undo"]
      ]
  ]

hasHistory :: FrontendState -> Boolean
hasHistory state = NEL.length (view _marloweState state) > 1

-- printTransWarnings :: forall p. Int -> List DynamicProblem -> Array (HTML p Query)
-- printTransWarnings _ Nil = []

-- printTransWarnings num (Cons NoProblem rest) = printTransWarnings (num + 1) rest

-- printTransWarnings num (Cons CommitNotMade rest) = Array.cons (text ("Input number " <> (show num) <> " will have no effect because the commitment has not been made yet.")) (printTransWarnings (num + 1) rest)

-- printTransWarnings num (Cons NotEnoughMoneyLeftInCommit rest) = Array.cons (text ("Input number " <> (show num) <> " will not have the expected effect because there is not enough money left in the commit.")) (printTransWarnings (num + 1) rest)

-- printTransWarnings num (Cons CommitIsExpired rest) = Array.cons (text ("Input number " <> (show num) <> " will have no effect because the commitment has expired already.")) (printTransWarnings (num + 1) rest)

-- TODO: Need to make these errors nice explanations - function in smeantics utils
printTransError :: forall p. TransactionError -> Array (HTML p Query)
printTransError error = [ul_ [li_ [text (show error)]]]

contractParsingError :: forall p. Boolean -> Array (HTML p Query)
contractParsingError true =
  [ div
      [ classes
          [ ClassName "invalid-transaction"
          , ClassName "input-composer"
          ]
      ]
      [ h2 [] [text ""]
      ]
  ]

contractParsingError false =
  [ div
      [ classes
          [ ClassName "invalid-transaction"
          , ClassName "input-composer"
          ]
      ]
      [ h2 [] [text "Cannot parse contract"]
      ]
  ]

transactionErrors :: forall p. Maybe TransactionError -> Array (HTML p Query)
transactionErrors Nothing = []
transactionErrors (Just error) =
  [ div
      [ classes
          [ ClassName "invalid-transaction"
          , ClassName "input-composer"
          ]
      ]
      ( [h2 [] [text "The transaction is invalid:"]]
        <> printTransError error
      )
  ]

-- signatures :: forall p. Map Person Boolean -> Boolean -> Map Person BigInteger -> Array (HTML p Query)
-- signatures people isEnabled outcomes =
--   [ h3_ [text "Signatures"]
--   , if ((Map.size people) == 0)
--       then div [] [text "No participants in contract"]
--       else
--         div
--           [ classes
--               [ ClassName "d-flex"
--               , ClassName "flex-row"
--               , ClassName "align-items-center"
--               , ClassName "justify-content-start"
--               ]
--           ] (map (\x -> signature x isEnabled outcomes) $ Map.toUnfoldable people)
--   ]

-- signature :: forall p. Tuple Person Boolean -> Boolean -> Map Person BigInteger -> HTML p Query
-- signature (Tuple person isChecked) isEnabled outcomes =
--   span
--     [ class_ $ ClassName "pr-2"
--     ]
--     [ input
--         [ type_ InputCheckbox
--         , onChecked $ Just <<< HQ.action
--             <<< ( \v ->
--                 SetSignature
--                   { person
--                   , isChecked: v
--                   }
--               )
--         , enabled isEnabled
--         , checked isChecked
--         ]
--     , span_
--         [ text $ " Person " <> show person
--         ]
--     , span [classes [ClassName "outcome-block"]]
--         [ text $ "(" <> outcome <> " ADA)"
--         ]
--     ]
--   where
--   outcome = case Map.lookup person outcomes of
--     Nothing -> "+0"
--     Just x -> if (x >= fromInt 0) then "+" <> show x else show x

transactionInputs :: forall p. MarloweState -> Array (HTML p Query)
transactionInputs state =
  [ h3_
      [ text "Input list"
      ]
  ]
    <> ( onEmpty [text "No inputs in the transaction"]
        $ map (inputRow isEnabled) (state ^. _pendingInputs)
      )
  where
  isEnabled = state.contract /= Nothing

inputRow :: forall p. Boolean -> Tuple3 Input PubKey Int -> HTML p Query
inputRow isEnabled (INotify /\ person /\ index /\ _) = 
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ input_ $ RemoveInput person index INotify
            ]
            [ text "-"
            ]
        , text "Notification"
        ]
    ]
inputRow isEnabled (input@(IDeposit accountId party money) /\ person /\ index /\ _) =
  row_
    [ col_ $
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ input_ $ RemoveInput person index input
            ]
            [ text "-"
            ]
        ] <> (renderDeposit accountId party (unwrap money))
    ]

inputRow isEnabled (input@(IChoice (ChoiceId { choiceNumber, choiceOwner}) chosenNum) /\ person /\ index /\ _) =
  row_
    [ col_
        [ button
            [ class_ $ ClassName "composer-add-button"
            , enabled isEnabled
            , onClick $ input_ $ RemoveInput person index input
            ]
            [ text "-"
            ]
        , text "Participant "
        , b_
            [ text (show choiceOwner)
            ]
        , text " chooses the value "
        , b_
            [ text (show chosenNum)
            ]
        , text " for choice with id "
        , b_
            [ text (show choiceNumber)
            ]
        ]
    ]

stateTitle ::
  forall p.
  FrontendState ->
  HTML p Query
stateTitle state =
  div
    [ classes
        [ ClassName "demos"
        , ClassName "d-flex"
        , ClassName "flex-row"
        , ClassName "align-items-center"
        , ClassName "justify-content-between"
        , ClassName "mt-3"
        , ClassName "mb-3"
        ]
    ]
    [ paneHeader "State"
    , span
        [ classes
            [ ClassName "btn"
            , ClassName "btn-sm"
            ]
        ]
        [ strong_
            [ text "Current Block:"
            ]
        , span
            [ class_ $ ClassName "block-number"
            ]
            [ view (_marloweState <<< _Head <<< _slot <<< to show <<< to text) state
            ]
        , strong_
            [ text "Money in contract:"
            ]
        , span
            [ class_ $ ClassName "money-in-contract"
            ]
            [ view (_marloweState <<< _Head <<< _moneyInContract <<< to show <<< to text) state
            ]
        , strong_ [text "ADA"]
        ]
    ]

statePane :: forall p. FrontendState -> HTML p Query
statePane state =
  div
    [ class_ $ ClassName "col"
    ]
    [ stateTable state
    ]

stateTable :: forall p. FrontendState -> HTML p Query
stateTable state =
  div
    [ class_ $ ClassName "full-width-card"
    ]
    [ card_
        [ cardBody_
            [ h3_
                [ text "Accounts"
                ]
            , row_
                [ if (Map.size accounts == 0)
                    then text "There are no accounts in the state"
                    else renderAccounts accounts
                ]
            , h3_
                [ text "Choices"
                ]
            , row_
                [ if (Map.size choices == 0)
                    then text "No choices have been recorded"
                    else renderChoices choices
                ]
            ]
        ]
    ]
  where
  accounts = state ^. _marloweState <<< _Head <<< _state <<< _accounts
  choices = state ^. _marloweState <<< _Head <<< _state <<< _choices

renderAccounts :: forall p. Map AccountId Ada -> HTML p Query
renderAccounts accounts =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Account id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th_
                [ text "Chosen value"
                ]
            ]
        ]
    , tbody_ (map renderAccount accountList)
    ]
  where
  accountList = Map.toUnfoldable accounts :: Array (Tuple AccountId Ada)

renderAccount :: forall p. Tuple AccountId Ada -> HTML p Query
renderAccount (Tuple (AccountId { accountNumber, accountOwner}) value) =
  tr []
    [ td_
        [ text (show accountNumber)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show accountOwner)
        ]
    , td_
        [ text (show value)
        ]
    ]


renderChoices :: forall p. Map ChoiceId ChosenNum -> HTML p Query
renderChoices choices =
  table_
    [ colgroup []
        [ col []
        , col []
        , col []
        ]
    , thead_
        [ tr []
            [ th_
                [ text "Choice id"
                ]
            , th
                [ class_ $ ClassName "middle-column"
                ]
                [ text "Participant"
                ]
            , th_
                [ text "Chosen value"
                ]
            ]
        ]
    , tbody_ (map renderChoice choiceList)
    ]
  where
  choiceList = Map.toUnfoldable choices :: Array (Tuple ChoiceId ChosenNum)

renderChoice :: forall p. Tuple ChoiceId ChosenNum -> HTML p Query
renderChoice (Tuple (ChoiceId { choiceNumber, choiceOwner}) value) =
  tr []
    [ td_
        [ text (show choiceNumber)
        ]
    , td
        [ class_ $ ClassName "middle-column"
        ]
        [ text (show choiceOwner)
        ]
    , td_
        [ text (show value)
        ]
    ]
