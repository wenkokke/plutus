{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Playground.API where

import           Control.Arrow                (left)
import           Control.Lens                 (over, _2)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.State    (StateT, evalStateT, get, put)
import           Control.Newtype.Generics     (Newtype)
import           Data.Aeson                   (FromJSON (parseJSON), ToJSON (toJSON), Value, withText)
import qualified Data.Aeson                   as JSON
import           Data.Bifunctor               (second)
import qualified Data.ByteString.Base64       as Base64
import           Data.ByteString.Lazy         (fromStrict, toStrict)
import qualified Data.HashMap.Strict.InsOrd   as HM
import           Data.Map                     (Map)
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Swagger                 (ParamSchema (ParamSchema), Referenced (Inline, Ref), Schema (Schema),
                                               SwaggerType (SwaggerInteger, SwaggerObject, SwaggerString))
import qualified Data.Swagger                 as Swagger
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (decodeUtf8, decodeUtf8', encodeUtf8)
import           GHC.Generics                 (Generic)
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.TH.Syntax   as TH
import           Network.HTTP.Media           ((//), (/:))
import           Servant.API                  ((:<|>), (:>), Accept (contentType), Capture, JSON,
                                               MimeRender (mimeRender), MimeUnrender (mimeUnrender), NoContent, Post,
                                               ReqBody)
import           Text.Read                    (readMaybe)
import           Wallet.Emulator.Types        (Wallet)
import           Wallet.UTXO.Types            (Blockchain)

type API
   = "contract" :> ReqBody '[ JSON] SourceCode :> Post '[ JSON] (Either [CompilationError] [FunctionSchema SimpleArgumentSchema])
     :<|> "evaluate" :> ReqBody '[ JSON] Evaluation :> Post '[ JSON] Blockchain

newtype SourceCode = SourceCode Text
  deriving stock (Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving anyclass (Newtype)

newtype Fn = Fn Text
  deriving stock (Show, Generic, TH.Lift)
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

data Evaluation = Evaluation
  { wallets    :: [(Wallet, Integer)]
  , program    :: Program
  , sourceCode :: SourceCode
  , blockchain :: Blockchain
  }
  deriving (Generic, ToJSON, FromJSON)

data FunctionSchema a = FunctionSchema
  { functionName   :: Fn
  , argumentSchema :: [a]
  } deriving (Show, Generic, ToJSON, Functor)

data SimpleArgumentSchema
  = SimpleIntArgument
  | SimpleStringArgument
  | SimpleObjectArgument [(Text, SimpleArgumentSchema)]
  | UnknownArgument Text
  deriving (Show, Eq, Generic, ToJSON)

toSimpleArgumentSchema :: Schema -> SimpleArgumentSchema
toSimpleArgumentSchema schema@Schema {..} =
  case _schemaParamSchema of
    ParamSchema {..} ->
      case _paramSchemaType of
        SwaggerInteger -> SimpleIntArgument
        SwaggerString -> SimpleStringArgument
        SwaggerObject ->
          SimpleObjectArgument $
          over
            _2
            (\case
               Inline v -> toSimpleArgumentSchema v
               Ref _ -> unknown) <$>
          HM.toList _schemaProperties
        _ -> unknown
  where
    unknown = UnknownArgument $ Text.pack $ show schema

------------------------------------------------------------

data CompilationError
  = RawError Text
  | CompilationError { filename :: !Text
                     , row      :: !Int
                     , column   :: !Int
                     , text     :: ![Text] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data PlaygroundError
  = CompilationErrors [CompilationError]
  | InterpreterError Hint.InterpreterError
  | FunctionSchemaError
  | DecodeJsonTypeError String String
  deriving stock (Show, Generic)

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
