module Types where

import Ace.Halogen.Component (AceMessage)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Either (Either(..))
import Data.Generic (GenericSignature(SigRecord, SigString, SigInt), GenericSpine(..))
import Data.Lens (Lens', Prism', _2, lens, over, prism, set, traversed)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.ECharts (EChartsMessage)
import Network.RemoteData (RemoteData)
import Playground.API (CompilationError, FunctionSchema, SimpleArgumentSchema(SimpleObjectArgument, UnknownArgument, SimpleStringArgument, SimpleIntArgument))
import Prelude (class Functor, Unit, bind, const, pure, unit, ($), (<$>), (<<<))
import Servant.PureScript.Affjax (AjaxError)
import Wallet.Emulator.Types (Wallet)
import Wallet.UTXO.Types (Tx)

type MockWallet =
  { wallet :: Wallet
  , balance :: Int
  }

_wallet :: forall s a. Lens' {wallet :: a | s} a
_wallet = prop (SProxy :: SProxy "wallet")

_balance :: forall s a. Lens' {balance :: a | s} a
_balance = prop (SProxy :: SProxy "balance")

------------------------------------------------------------

type Action =
  { mockWallet :: MockWallet
  , functionSchema :: FunctionSchema SimpleArgument
  }

------------------------------------------------------------

data Query a
  = HandleAceMessage AceMessage a
  | HandleEChartsMessage EChartsMessage a
  | CompileProgram a
  | ScrollTo { row :: Int, column :: Int } a
  | AddWallet a
  | RemoveWallet Int a
  | AddAction Action a
  | RemoveAction Int a
  | EvaluateActions a
  | PopulateAction Int Int (FormEvent a)

data FormEvent a
  = SetIntField Int a
  | SetStringField String a
  | SetSubField Int (FormEvent a)

derive instance functorFormEvent :: Functor FormEvent

instance extendFormEvent :: Extend FormEvent where
  extend f event@(SetIntField n _) = SetIntField n $ f event
  extend f event@(SetStringField s _) = SetStringField s $ f event
  extend f event@(SetSubField n _) = SetSubField n $ extend f event


instance comonadFormEvent :: Comonad FormEvent where
  extract (SetIntField _ a) = a
  extract (SetStringField _ a) = a
  extract (SetSubField _ e) = extract e

-----------------------------------------------------------

type CompilationResult =
  Either (Array CompilationError) (Array (FunctionSchema SimpleArgumentSchema))

type Blockchain = Array (Array Tx)

type State =
  { editorContents :: String
  , compilationResult :: RemoteData AjaxError CompilationResult
  , wallets :: Array MockWallet
  , actions :: Array Action
  , evaluationResult :: RemoteData AjaxError Blockchain
  }

_actions :: forall s a. Lens' {actions :: a | s} a
_actions = prop (SProxy :: SProxy "actions")

_wallets :: forall s a. Lens' {wallets :: a | s} a
_wallets = prop (SProxy :: SProxy "wallets")

_evaluationResult :: forall s a. Lens' {evaluationResult :: a | s} a
_evaluationResult = prop (SProxy :: SProxy "evaluationResult")

_editorContents :: forall s a. Lens' {editorContents :: a | s} a
_editorContents = prop (SProxy :: SProxy "editorContents")

_compilationResult :: forall s a. Lens' {compilationResult :: a | s} a
_compilationResult = prop (SProxy :: SProxy "compilationResult")

------------------------------------------------------------

type Balance =
  { name :: String
  , value :: Number
  }

type Transfer =
  { source :: String
  , target :: String
  , value :: Number
  }


data SimpleArgument
  = SimpleInt (Maybe Int)
  | SimpleString (Maybe String)
  | SimpleObject (Array (Tuple String SimpleArgument))
  | Unknowable

toValue :: SimpleArgumentSchema -> SimpleArgument
toValue SimpleIntArgument = SimpleInt Nothing
toValue SimpleStringArgument = SimpleString Nothing
toValue (SimpleObjectArgument fields) = SimpleObject (over (traversed <<< _2) toValue fields)
toValue (UnknownArgument _) = Unknowable

toSignature :: SimpleArgumentSchema -> Either String GenericSignature
toSignature SimpleIntArgument = Right SigInt
toSignature SimpleStringArgument = Right SigString
toSignature (UnknownArgument err) = Left err
toSignature (SimpleObjectArgument fields) = do
  subfields <- traverse toSignatureField fields
  pure $ SigRecord subfields
  where
    toSignatureField :: Tuple String SimpleArgumentSchema -> Either String { recLabel :: String, recValue :: Unit -> GenericSignature }
    toSignatureField (Tuple recLabel value) = do
      recValue <- const <$> toSignature value
      pure { recLabel, recValue }


data SetArgument
  = SetArgumentLeaf GenericSignature
  | SetArgumentField Int String GenericSignature
  | SetArgumentNumber Int GenericSignature

_SInt :: Prism' GenericSpine Int
_SInt = prism SInt case _ of
  SInt x -> Right x
  other -> Left other

_SString :: Prism' GenericSpine String
_SString = prism SString case _ of
  SString x -> Right x
  other -> Left other

type SField =
  { recLabel :: String
  , recValue :: Unit -> GenericSpine
  }

_SRecord :: Prism' GenericSpine (Array SField)
_SRecord = prism SRecord case _ of
  SRecord fields -> Right fields
  other -> Left other

_recLabel :: Lens' SField String
_recLabel = prop (SProxy :: SProxy "recLabel")

_recValue :: Lens' SField GenericSpine
_recValue = lens get set
  where
    get r = r.recValue unit
    set r v = r { recValue = const v }

k :: Array Int
k = set (ix 1) 5 [1,2,3]
--j = set (at 3 <<< _Just <<< _SInt) 5 []

-- | This should just be `map` but we can't put an orphan instance on FunctionSchema. :-(
toValueLevel :: FunctionSchema SimpleArgumentSchema -> FunctionSchema SimpleArgument
toValueLevel = over (_Newtype <<< prop (SProxy :: SProxy "argumentSchema") <<< traversed) toValue
