{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Playground.API where

import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.State    (StateT, evalStateT, get, put)
import           Control.Newtype.Generics     (pack, unpack)
import           Data.Aeson                   (FromJSON, ToJSON, Value)
import           Data.Bifunctor               (second)
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics                 (Generic)
import           Language.Haskell.Interpreter (CompilationError (CompilationError, RawError), SourceCode, column,
                                               filename, row, text)
import qualified Language.Haskell.Interpreter as HI
import qualified Language.Haskell.TH.Syntax   as TH
import           Ledger.Schema                (SimpleArgumentSchema (SimpleArraySchema, SimpleIntSchema, SimpleObjectSchema, SimpleStringSchema, SimpleTupleSchema, SimpleTypeSchema, UnknownSchema))
import           Ledger.Types                 (Blockchain, PubKey, Tx, TxId)
import           Ledger.Validation            (ValidatorHash)
import qualified Ledger.Value                 as V
import           Servant.API                  ((:<|>), (:>), Get, JSON, Post, ReqBody)
import           Text.Read                    (readMaybe)
import           Wallet.Emulator.Types        (EmulatorEvent, Wallet)
import           Wallet.Graph                 (FlowGraph)

type API
   = "contract" :> ReqBody '[ JSON] SourceCode :> Post '[ JSON] (Either HI.InterpreterError CompilationResult)
     :<|> "evaluate" :> ReqBody '[ JSON] Evaluation :> Post '[ JSON] EvaluationResult
     :<|> "health" :> Get '[ JSON] ()

-- FIXME: These types will be defined elsewhere but I've added them here for now
newtype TokenId = TokenId Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

data KnownCurrency = KnownCurrency
    { hash         :: ValidatorHash
    , friendlyName :: String
    , knownTokens  :: NonEmpty TokenId
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
--------------------------------------------------------------------------------

newtype Fn = Fn Text
  deriving stock (Eq, Show, Generic, TH.Lift)
  deriving newtype (ToJSON, FromJSON)

data Expression
  = Action
    { function  :: Fn
    , wallet    :: Wallet
    , arguments :: [Value]
    }
  | Wait { blocks :: Int }
  deriving (Show, Generic, ToJSON, FromJSON)

type Program = [Expression]

data SimulatorWallet = SimulatorWallet
  { simulatorWalletWallet  :: Wallet
  , simulatorWalletBalance :: V.Value
  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Evaluation = Evaluation
  { wallets    :: [SimulatorWallet]
  , program    :: Program
  , sourceCode :: SourceCode
  , blockchain :: Blockchain
  }
  deriving (Generic, ToJSON, FromJSON)

pubKeys :: Evaluation -> [PubKey]
pubKeys Evaluation{..} = pack . unpack . simulatorWalletWallet <$> wallets

data EvaluationResult = EvaluationResult
  { resultBlockchain  :: [[(TxId, Tx)]] -- Blockchain annotated with hashes.
  , resultGraph       :: FlowGraph
  , emulatorLog       :: [EmulatorEvent]
  , fundsDistribution :: [SimulatorWallet]
  }
  deriving (Generic, ToJSON)

newtype Warning = Warning Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON)

data CompilationResult = CompilationResult
  { functionSchema  :: [FunctionSchema SimpleArgumentSchema]
  , knownCurrencies :: [KnownCurrency]
  , warnings        :: [Warning]
  }
  deriving (Show, Generic, ToJSON)

data FunctionSchema a = FunctionSchema
  { functionName   :: Fn
  , argumentSchema :: [a]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, Functor)

-- | We could ship the Swagger Schema to the frontend and work with
-- that directly. But it's verbose and pretty complicated, and
-- supports expressions we have no intention of supporting. So, we
-- convert from Swagger to a much simpler schema type.
--
-- But because the Swagger schema is more expressive, there will be
-- cases where we can't convert. We don't give up. We don't just
-- return an error. Instead we transfer a simplified version of, "We
-- don't know what this Swagger schema represents." This way if
-- there's a nested structure we don't understand, we can tell the
-- frontend in detail. We can say, "We understood the whole object
-- except for this particular field." Then there's some feedback for
-- the user to change that field's type. Much more useful than
-- rejecting the entire description.

isSupportedByFrontend :: SimpleArgumentSchema -> Bool
isSupportedByFrontend SimpleIntSchema = True
isSupportedByFrontend SimpleStringSchema = True
isSupportedByFrontend (SimpleObjectSchema _ subSchema) =
    all isSupportedByFrontend (snd <$> subSchema)
isSupportedByFrontend (SimpleTypeSchema _ subSchema) =
    all isSupportedByFrontend subSchema
isSupportedByFrontend (SimpleArraySchema subSchema) =
    isSupportedByFrontend subSchema
isSupportedByFrontend (SimpleTupleSchema (subSchemaX, subSchemaY)) =
    all isSupportedByFrontend [subSchemaX, subSchemaY]
isSupportedByFrontend (UnknownSchema _ _) = False
------------------------------------------------------------

data PlaygroundError
  = CompilationErrors [CompilationError]
  | InterpreterError HI.InterpreterError
  | FunctionSchemaError
  | DecodeJsonTypeError String String
  | PlaygroundTimeout
  | OtherError String
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

parseErrorsText :: Text -> [CompilationError]
parseErrorsText input = parseErrorText <$> Text.splitOn "\n\n" input

parseErrorText :: Text -> CompilationError
parseErrorText input =
  fromMaybe (RawError input) $ flip evalStateT input $ do
    filename <- consumeTo ":"
    rowStr <- consumeTo ":"
    columnStr <- consumeTo ":"
    text <- Text.lines <$> consume
  --
    row <- lift $ readMaybe $ Text.unpack rowStr
    column <- lift $ readMaybe $ Text.unpack columnStr
    pure CompilationError {..}

consumeTo :: Monad m => Text -> StateT Text m Text
consumeTo needle = do
  (before, after) <- breakWith needle <$> get
  put after
  pure before

consume :: (Monad m, Monoid s) => StateT s m s
consume = get <* put mempty

-- | Light `Data.Text.breakOn`, but consumes the breakpoint text (the 'needle').
breakWith :: Text -> Text -> (Text, Text)
breakWith needle = second (Text.drop 1) . Text.breakOn needle
