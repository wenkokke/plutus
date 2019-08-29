module Types where

import Prelude

import Ace.Halogen.Component (AceMessage, AceQuery)
import Auth (AuthStatus)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Cursor (Cursor)
import Data.Array (elem, mapWithIndex)
import Data.Array as Array
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, Lens', Prism', lens, prism', to, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.RawJson (JsonEither, JsonTuple(..), RawJson(..))
import Data.String.Extra (toHex) as String
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (aesonSumEncoding, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import Foreign.Object as FO
import Gist (Gist)
import Halogen.Chartist (ChartistMessage, ChartistQuery)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Language.Haskell.Interpreter (SourceCode, InterpreterError, InterpreterResult)
import Language.PlutusTx.AssocMap as AssocMap
import Ledger.Crypto (PubKey, _PubKey)
import Ledger.Interval (Extended(..), Interval(..), LowerBound(..), UpperBound(..))
import Ledger.Slot (Slot)
import Ledger.Tx (Tx)
import Ledger.TxId (TxIdOf)
import Ledger.Value (CurrencySymbol, TokenName, Value, _CurrencySymbol, _TokenName, _Value)
import Matryoshka (class Corecursive, class Recursive, Algebra, ana, cata)
import Network.RemoteData (RemoteData)
import Playground.API (CompilationResult, Evaluation(..), EvaluationResult, FunctionSchema, KnownCurrency, SimulatorWallet, _FunctionSchema, _SimulatorWallet)
import Playground.API as API
import Schema (FormSchema(..))
import Servant.PureScript.Ajax (AjaxError)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen
import Validation (class Validation, ValidationError(..), WithPath, addPath, noPath, validate)
import Wallet.Emulator.Types (Wallet, _Wallet)
import Web.HTML.Event.DragEvent (DragEvent)

_simulatorWallet :: forall r a. Lens' { simulatorWallet :: a | r } a
_simulatorWallet = prop (SProxy :: SProxy "simulatorWallet")

_simulatorWalletWallet :: Lens' SimulatorWallet Wallet
_simulatorWalletWallet = _SimulatorWallet <<< prop (SProxy :: SProxy "simulatorWalletWallet")

_simulatorWalletBalance :: Lens' SimulatorWallet Value
_simulatorWalletBalance = _SimulatorWallet <<< prop (SProxy :: SProxy "simulatorWalletBalance")

_walletId :: Lens' Wallet Int
_walletId = _Wallet <<< prop (SProxy :: SProxy "getWallet")

_pubKey :: Lens' PubKey String
_pubKey = _PubKey <<< prop (SProxy :: SProxy "getPubKey")

_value :: Lens' Value (AssocMap.Map CurrencySymbol (AssocMap.Map TokenName Int))
_value = _Value <<< prop (SProxy :: SProxy "getValue")

_currencySymbol :: Lens' CurrencySymbol String
_currencySymbol = _CurrencySymbol <<< prop (SProxy :: SProxy "unCurrencySymbol")

_tokenName :: Lens' TokenName String
_tokenName = _TokenName <<< prop (SProxy :: SProxy "unTokenName")

defaultJsonOptions :: Options
defaultJsonOptions =
  defaultOptions
    { unwrapSingleConstructors = true
    , sumEncoding = aesonSumEncoding
    }

data Action
  = Action
    { simulatorWallet :: SimulatorWallet
    , functionSchema :: FunctionSchema FormArgument
    }
  | Wait { blocks :: Int }

derive instance genericAction :: Generic Action _

derive instance eqAction :: Eq Action

instance encodeAction :: Encode Action where
  encode value = genericEncode defaultJsonOptions value

instance decodeAction :: Decode Action where
  decode value = genericDecode defaultJsonOptions value

_Action ::
  Prism'
    Action
    { simulatorWallet :: SimulatorWallet
    , functionSchema :: FunctionSchema FormArgument
    }
_Action = prism' Action f
  where
  f (Action r) = Just r

  f _ = Nothing

_Wait ::
  Prism'
    Action
    { blocks :: Int
    }
_Wait = prism' Wait f
  where
  f (Wait r) = Just r

  f _ = Nothing

_functionSchema :: forall a b r. Lens { functionSchema :: a | r } { functionSchema :: b | r } a b
_functionSchema = prop (SProxy :: SProxy "functionSchema")

_argumentSchema :: forall a b r. Lens { argumentSchema :: a | r } { argumentSchema :: b | r } a b
_argumentSchema = prop (SProxy :: SProxy "argumentSchema")

_functionName :: forall a b r. Lens { functionName :: a | r } { functionName :: b | r } a b
_functionName = prop (SProxy :: SProxy "functionName")

_blocks :: forall a b r. Lens { blocks :: a | r } { blocks :: b | r } a b
_blocks = prop (SProxy :: SProxy "blocks")

_ivFrom :: forall a r. Lens' { ivFrom :: a | r } a
_ivFrom = prop (SProxy :: SProxy "ivFrom")

_ivTo :: forall a r. Lens' { ivTo :: a | r } a
_ivTo = prop (SProxy :: SProxy "ivTo")

_LowerBoundExtended :: forall a. Lens' (LowerBound a) (Extended a)
_LowerBoundExtended = lens get set
  where
    get (LowerBound e _) = e
    set (LowerBound _ i) e = LowerBound e i

_LowerBoundInclusive :: forall a. Lens' (LowerBound a) Boolean
_LowerBoundInclusive = lens get set
  where
    get (LowerBound _ i) = i
    set (LowerBound e _) i = LowerBound e i

_UpperBoundExtended :: forall a. Lens' (UpperBound a) (Extended a)
_UpperBoundExtended = lens get set
  where
    get (UpperBound e _) = e
    set (UpperBound _ i) e = UpperBound e i

_UpperBoundInclusive :: forall a. Lens' (UpperBound a) Boolean
_UpperBoundInclusive = lens get set
  where
    get (UpperBound _ i) = i
    set (UpperBound e _) i = UpperBound e i

_a :: forall a r. Lens' { a :: a | r } a
_a = prop (SProxy :: SProxy "a")

-- | Any type that contains an `Extended a` value and an inclusive/exclusive flag.
class HasBound a v | a -> v where
  hasBound :: a -> Extended v
  isInclusive :: a -> Boolean

instance lowerBoundHasBound :: HasBound (LowerBound v) v where
  hasBound (LowerBound x _) = x
  isInclusive (LowerBound _ x) = x

instance upperBoundHasBound :: HasBound (UpperBound v) v where
  hasBound (UpperBound x _) = x
  isInclusive (UpperBound _ x) = x

instance actionValidation :: Validation Action where
  validate (Wait _) = []
  validate (Action action) = Array.concat $ Array.mapWithIndex (\i v -> addPath (show i) <$> validate v) args
    where
    args :: Array FormArgument
    args = view (_functionSchema <<< _FunctionSchema <<< _argumentSchema) action

------------------------------------------------------------
-- | TODO: It should always be true that either toExpression returns a
-- `Just value` OR validate returns a non-empty array.
-- This suggests they should be the same function, returning either a group of error messages, or a valid expression.
toExpression :: Action -> Maybe API.Expression
toExpression (Wait wait) = Just $ API.Wait wait

toExpression (Action action) = do
  let
    wallet = view _simulatorWalletWallet action.simulatorWallet
  arguments <- jsonArguments
  pure $ API.Action { wallet, function, arguments }
  where
  function = view (_functionSchema <<< to unwrap <<< _functionName) action

  argumentSchema = view (_functionSchema <<< to unwrap <<< _argumentSchema) action

  jsonArguments = traverse (map (RawJson <<< encodeJSON) <<< formArgumentToJson) argumentSchema

toEvaluation :: SourceCode -> Simulation -> Maybe Evaluation
toEvaluation sourceCode (Simulation { actions, wallets }) = do
  program <- traverse toExpression actions
  pure
    $ Evaluation
        { wallets
        , program
        , sourceCode
        , blockchain: []
        }

------------------------------------------------------------
data Query a
  -- SubEvents.
  = HandleEditorMessage AceMessage a
  | HandleDragEvent DragEvent a
  | ActionDragAndDrop Int DragAndDropEventType DragEvent a
  | HandleDropEvent DragEvent a
  | HandleBalancesChartMessage ChartistMessage a
  -- Gist support.
  | CheckAuthStatus a
  | PublishGist a
  | SetGistUrl String a
  | LoadGist a
  -- Tabs.
  | ChangeView View a
  -- Editor.
  | LoadScript String a
  | CompileProgram a
  | ScrollTo { row :: Int, column :: Int } a
  -- Simulations
  | AddSimulationSlot a
  | SetSimulationSlot Int a
  | RemoveSimulationSlot Int a
  -- Wallets.
  | ModifyWallets WalletEvent a
  -- Actions.
  | ModifyActions ActionEvent a
  | EvaluateActions a
  | PopulateAction Int Int (FormEvent a)

data WalletEvent
  = AddWallet
  | RemoveWallet Int
  | ModifyBalance Int ValueEvent

data ValueEvent
  = SetBalance CurrencySymbol TokenName Int

data ActionEvent
  = AddAction Action
  | AddWaitAction Int
  | RemoveAction Int
  | SetWaitTime Int Int

data DragAndDropEventType
  = DragStart
  | DragEnd
  | DragEnter
  | DragOver
  | DragLeave
  | Drop

instance showDragAndDropEventType :: Show DragAndDropEventType where
  show DragStart = "DragStart"
  show DragEnd = "DragEnd"
  show DragEnter = "DragEnter"
  show DragOver = "DragOver"
  show DragLeave = "DragLeave"
  show Drop = "Drop"

data FormEvent a
  = SetIntField (Maybe Int) a
  | SetBoolField Boolean a
  | SetStringField String a
  | SetHexField String a
  | SetRadioField String a
  | SetValueField ValueEvent a
  | SetSlotRangeField (Interval Slot) a
  | AddSubField a
  | SetSubField Int (FormEvent a)
  | RemoveSubField Int a

derive instance functorFormEvent :: Functor FormEvent

instance extendFormEvent :: Extend FormEvent where
  extend f event@(SetIntField n _) = SetIntField n $ f event
  extend f event@(SetBoolField n _) = SetBoolField n $ f event
  extend f event@(SetStringField s _) = SetStringField s $ f event
  extend f event@(SetHexField s _) = SetHexField s $ f event
  extend f event@(SetRadioField s _) = SetRadioField s $ f event
  extend f event@(SetValueField e _) = SetValueField e $ f event
  extend f event@(SetSlotRangeField e _) = SetSlotRangeField e $ f event
  extend f event@(AddSubField _) = AddSubField $ f event
  extend f event@(SetSubField n _) = SetSubField n $ extend f event
  extend f event@(RemoveSubField n _) = RemoveSubField n $ f event

instance comonadFormEvent :: Comonad FormEvent where
  extract (SetIntField _ a) = a
  extract (SetBoolField _ a) = a
  extract (SetStringField _ a) = a
  extract (SetHexField _ a) = a
  extract (SetRadioField _ a) = a
  extract (SetValueField _ a) = a
  extract (SetSlotRangeField _ a) = a
  extract (AddSubField a) = a
  extract (SetSubField _ e) = extract e
  extract (RemoveSubField _ e) = e

------------------------------------------------------------
type ChildQuery
  = Coproduct2 AceQuery ChartistQuery

type ChildSlot
  = Either2 EditorSlot BalancesChartSlot

data EditorSlot
  = EditorSlot

derive instance eqComponentEditorSlot :: Eq EditorSlot

derive instance ordComponentEditorSlot :: Ord EditorSlot

data BalancesChartSlot
  = BalancesChartSlot

derive instance eqComponentBalancesChartSlot :: Eq BalancesChartSlot

derive instance ordComponentBalancesChartSlot :: Ord BalancesChartSlot

cpEditor :: ChildPath AceQuery ChildQuery EditorSlot ChildSlot
cpEditor = cp1

cpBalancesChart :: ChildPath ChartistQuery ChildQuery BalancesChartSlot ChildSlot
cpBalancesChart = cp2

-----------------------------------------------------------
type Blockchain
  = Array (Array (JsonTuple (TxIdOf String) Tx))

type Signatures
  = Array (FunctionSchema FormSchema)

newtype Simulation
  = Simulation
  { signatures :: Signatures
  , actions :: Array Action
  , wallets :: Array SimulatorWallet
  , currencies :: Array KnownCurrency
  }

derive instance newtypeSimulation :: Newtype Simulation _

derive instance genericSimulation :: Generic Simulation _

instance encodeSimulation :: Encode Simulation where
  encode value = genericEncode defaultJsonOptions value

instance decodeSimulation :: Decode Simulation where
  decode value = genericDecode defaultJsonOptions value

type WebData
  = RemoteData AjaxError

newtype State
  = State
  { currentView :: View
  , compilationResult :: WebData (JsonEither InterpreterError (InterpreterResult CompilationResult))
  , simulations :: Cursor Simulation
  , actionDrag :: Maybe Int
  , evaluationResult :: WebData EvaluationResult
  , authStatus :: WebData AuthStatus
  , createGistResult :: WebData Gist
  , gistUrl :: Maybe String
  }

derive instance newtypeState :: Newtype State _

_currentView :: Lens' State View
_currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

_simulations :: Lens' State (Cursor Simulation)
_simulations = _Newtype <<< prop (SProxy :: SProxy "simulations")

_actionDrag :: Lens' State (Maybe Int)
_actionDrag = _Newtype <<< prop (SProxy :: SProxy "actionDrag")

_signatures :: Lens' Simulation Signatures
_signatures = _Newtype <<< prop (SProxy :: SProxy "signatures")

_actions :: Lens' Simulation (Array Action)
_actions = _Newtype <<< prop (SProxy :: SProxy "actions")

_wallets :: Lens' Simulation (Array SimulatorWallet)
_wallets = _Newtype <<< prop (SProxy :: SProxy "wallets")

_evaluationResult :: Lens' State (WebData EvaluationResult)
_evaluationResult = _Newtype <<< prop (SProxy :: SProxy "evaluationResult")

_compilationResult :: Lens' State (WebData (JsonEither InterpreterError (InterpreterResult CompilationResult)))
_compilationResult = _Newtype <<< prop (SProxy :: SProxy "compilationResult")

_authStatus :: Lens' State (WebData AuthStatus)
_authStatus = _Newtype <<< prop (SProxy :: SProxy "authStatus")

_createGistResult :: Lens' State (WebData Gist)
_createGistResult = _Newtype <<< prop (SProxy :: SProxy "createGistResult")

_gistUrl :: Lens' State (Maybe String)
_gistUrl = _Newtype <<< prop (SProxy :: SProxy "gistUrl")

_resultBlockchain :: Lens' EvaluationResult Blockchain
_resultBlockchain = _Newtype <<< prop (SProxy :: SProxy "resultBlockchain")

_knownCurrencies :: Lens' CompilationResult (Array KnownCurrency)
_knownCurrencies = _Newtype <<< prop (SProxy :: SProxy "knownCurrencies")

data View
  = Editor
  | Simulations
  | Transactions

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (Editor :| [ Simulations, Transactions ])

instance showView :: Show View where
  show Editor = "Editor"
  show Simulations = "Simulation"
  show Transactions = "Transactions"

------------------------------------------------------------
data FormArgument
  = FormInt (Maybe Int)
  | FormBool Boolean
  | FormString (Maybe String)
  | FormHex (Maybe String)
  | FormRadio (Array String) (Maybe String)
  | FormArray FormSchema (Array FormArgument)
  | FormMaybe FormSchema (Maybe FormArgument)
  | FormTuple (JsonTuple FormArgument FormArgument)
  | FormObject (Array (JsonTuple String FormArgument))
  | FormValue Value
  | FormSlotRange (Interval Slot)
  | FormUnsupported { description :: String }

derive instance genericFormArgument :: Generic FormArgument _

derive instance eqFormArgument :: Eq FormArgument

instance showFormArgument :: Show FormArgument where
  show x = genericShow x

instance encodeFormArgument :: Encode FormArgument where
  encode value = genericEncode defaultJsonOptions value

instance decodeFormArgument :: Decode FormArgument where
  decode value = genericDecode defaultJsonOptions value

toArgument :: Value -> FormSchema -> FormArgument
toArgument initialValue = ana algebra
  where
  algebra :: FormSchema -> FormArgumentF FormSchema
  algebra FormSchemaInt = FormIntF Nothing

  algebra FormSchemaBool = FormBoolF false

  algebra FormSchemaString = FormStringF Nothing

  algebra FormSchemaHex = FormHexF Nothing

  algebra (FormSchemaRadio xs) = FormRadioF xs Nothing

  algebra (FormSchemaArray xs) = FormArrayF xs []

  algebra (FormSchemaMaybe x) = FormMaybeF x Nothing

  algebra FormSchemaValue = FormValueF initialValue

  algebra FormSchemaSlotRange = FormSlotRangeF defaultSlotRange

  algebra (FormSchemaTuple a b) = FormTupleF (JsonTuple (Tuple a b))

  algebra (FormSchemaObject xs) = FormObjectF xs

  algebra (FormSchemaUnsupported x) = FormUnsupportedF x

defaultSlotRange :: Interval Slot
defaultSlotRange =
  Interval
    { ivFrom: LowerBound NegInf true
    , ivTo: UpperBound PosInf true
    }

------------------------------------------------------------
-- | This type serves as a functorised version of `FormArgument` so
-- we can do some recursive processing of the data without cluttering
-- the transformation with the iteration.
data FormArgumentF a
  = FormIntF (Maybe Int)
  | FormBoolF Boolean
  | FormStringF (Maybe String)
  | FormHexF (Maybe String)
  | FormRadioF (Array String) (Maybe String)
  | FormTupleF (JsonTuple a a)
  | FormArrayF FormSchema (Array a)
  | FormMaybeF FormSchema (Maybe a)
  | FormObjectF (Array (JsonTuple String a))
  | FormValueF Value
  | FormSlotRangeF (Interval Slot)
  | FormUnsupportedF { description :: String }

instance functorFormArgumentF :: Functor FormArgumentF where
  map f (FormIntF x) = FormIntF x
  map f (FormBoolF x) = FormBoolF x
  map f (FormStringF x) = FormStringF x
  map f (FormHexF x) = FormHexF x
  map f (FormRadioF options x) = FormRadioF options x
  map f (FormTupleF (JsonTuple (Tuple x y))) = FormTupleF (JsonTuple (Tuple (f x) (f y)))
  map f (FormArrayF schema xs) = FormArrayF schema (map f xs)
  map f (FormMaybeF schema x) = FormMaybeF schema (map f x)
  map f (FormObjectF xs) = FormObjectF (map (map f) xs)
  map f (FormSlotRangeF x) = FormSlotRangeF x
  map f (FormValueF x) = FormValueF x
  map f (FormUnsupportedF x) = FormUnsupportedF x

derive instance eqFormArgumentF :: Eq a => Eq (FormArgumentF a)

instance recursiveFormArgument :: Recursive FormArgument FormArgumentF where
  project (FormInt x) = FormIntF x
  project (FormBool x) = FormBoolF x
  project (FormString x) = FormStringF x
  project (FormHex x) = FormHexF x
  project (FormRadio options x) = FormRadioF options x
  project (FormTuple x) = FormTupleF x
  project (FormArray schema xs) = FormArrayF schema xs
  project (FormMaybe schema x) = FormMaybeF schema x
  project (FormObject xs) = FormObjectF xs
  project (FormValue x) = FormValueF x
  project (FormSlotRange x) = FormSlotRangeF x
  project (FormUnsupported x) = FormUnsupportedF x

instance corecursiveFormArgument :: Corecursive FormArgument FormArgumentF where
  embed (FormIntF x) = FormInt x
  embed (FormBoolF x) = FormBool x
  embed (FormStringF x) = FormString x
  embed (FormHexF x) = FormHex x
  embed (FormRadioF options x) = FormRadio options x
  embed (FormTupleF xs) = FormTuple xs
  embed (FormArrayF schema xs) = FormArray schema xs
  embed (FormMaybeF schema x) = FormMaybe schema x
  embed (FormObjectF xs) = FormObject xs
  embed (FormValueF x) = FormValue x
  embed (FormSlotRangeF x) = FormSlotRange x
  embed (FormUnsupportedF x) = FormUnsupported x

------------------------------------------------------------
instance validationFormArgument :: Validation FormArgument where
  validate = cata algebra
    where
    algebra :: Algebra FormArgumentF (Array (WithPath ValidationError))
    algebra (FormIntF (Just _)) = []

    algebra (FormIntF Nothing) = [ noPath Required ]

    algebra (FormBoolF _) = []

    algebra (FormStringF (Just _)) = []

    algebra (FormStringF Nothing) = [ noPath Required ]

    algebra (FormHexF (Just _)) = []

    algebra (FormHexF Nothing) = [ noPath Required ]

    algebra (FormRadioF options (Just x)) =
      if x `elem` options then
        []
      else
        [ noPath Invalid ]

    algebra (FormRadioF _ Nothing) = [ noPath Required ]

    algebra (FormTupleF (JsonTuple (Tuple xs ys))) =
      Array.concat
        [ addPath "_1" <$> xs
        , addPath "_2" <$> ys
        ]

    algebra (FormMaybeF _ Nothing) = [ noPath Required ]

    algebra (FormMaybeF _ (Just x)) = addPath "_Just" <$> x

    algebra (FormArrayF _ xs) = Array.concat $ mapWithIndex (\i values -> addPath (show i) <$> values) xs

    algebra (FormObjectF xs) = Array.concat $ map (\(JsonTuple (Tuple name values)) -> addPath name <$> values) xs

    algebra (FormValueF x) = []

    algebra (FormSlotRangeF _) = []

    algebra (FormUnsupportedF _) = [ noPath Unsupported ]

formArgumentToJson :: FormArgument -> Maybe Foreign
formArgumentToJson arg = cata algebra arg
  where
  algebra :: Algebra FormArgumentF (Maybe Foreign)
  algebra (FormIntF (Just n)) = Just $ encode n

  algebra (FormIntF Nothing) = Nothing

  algebra (FormBoolF b) = Just $ encode b

  algebra (FormStringF (Just str)) = Just $ encode str

  algebra (FormStringF Nothing) = Nothing

  algebra (FormRadioF _ (Just option)) = Just $ encode option

  algebra (FormRadioF _ Nothing) = Nothing

  algebra (FormHexF (Just str)) = Just $ encode $ String.toHex str

  algebra (FormHexF Nothing) = Nothing

  algebra (FormTupleF (JsonTuple (Just fieldA /\ Just fieldB))) = Just $ encode [ fieldA, fieldB ]

  algebra (FormTupleF _) = Nothing

  algebra (FormMaybeF _ field) = encode <$> field

  algebra (FormArrayF _ fields) = Just $ encode fields

  algebra (FormObjectF fields) = encodeFields fields
    where
    encodeFields :: Array (JsonTuple String (Maybe Foreign)) -> Maybe Foreign
    encodeFields xs = map (encode <<< FO.fromFoldable) $ prepareObject xs

    prepareObject :: Array (JsonTuple String (Maybe Foreign)) -> Maybe (Array (Tuple String Foreign))
    prepareObject = traverse processTuples

    processTuples :: JsonTuple String (Maybe Foreign) -> Maybe (Tuple String Foreign)
    processTuples = unwrap >>> sequence

  algebra (FormValueF x) = Just $ encode x

  algebra (FormSlotRangeF x) = Just $ encode x

  algebra (FormUnsupportedF _) = Nothing

--- Language.Haskell.Interpreter ---
_result :: forall s a. Lens' { result :: a | s } a
_result = prop (SProxy :: SProxy "result")

_warnings :: forall s a. Lens' { warnings :: a | s } a
_warnings = prop (SProxy :: SProxy "warnings")
