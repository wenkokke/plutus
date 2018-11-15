module Types where

import Ace.Halogen.Component (AceMessage)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Halogen.ECharts (EChartsMessage)
import Network.RemoteData (RemoteData)
import Playground.API (CompilationError, FunctionSchema, SimpleArgumentSchema)
import Servant.PureScript.Affjax (AjaxError)
import Wallet.UTXO.Types (Tx)

newtype WalletId = WalletId String
derive instance newtypeWalletId :: Newtype WalletId _

type DummyWallet =
  { walletId :: WalletId
  , balance :: Number
  }

_walletId :: forall s a. Lens' {walletId :: a | s} a
_walletId = prop (SProxy :: SProxy "walletId")

_balance :: forall s a. Lens' {balance :: a | s} a
_balance = prop (SProxy :: SProxy "balance")

------------------------------------------------------------

type Action =
  { walletId :: WalletId
  , functionSchema :: FunctionSchema SimpleArgumentSchema
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

-----------------------------------------------------------

type CompilationResult =
  Either (Array CompilationError) (Array (FunctionSchema SimpleArgumentSchema))

type Blockchain = Array (Array Tx)

type State =
  { editorContents :: String
  , compilationResult :: RemoteData AjaxError CompilationResult
  , wallets :: Array DummyWallet
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
