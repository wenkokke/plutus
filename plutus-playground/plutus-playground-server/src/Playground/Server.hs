{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Playground.Server
  ( mkHandlers
  ) where

import           Control.Concurrent.MVar             (MVar, newMVar, withMVar)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Newtype.Generics            (Newtype, unpack)
import           Data.Aeson                          (encode)
import qualified Data.ByteString.Lazy.Char8          as BSL
import           Data.Maybe                          (catMaybes, fromMaybe)
import qualified Data.Swagger                        as Swagger
import qualified Data.Text                           as Text
import           GHC.Generics                        (Generic)
import           Language.Haskell.Interpreter        (InterpreterError (WontCompile), InterpreterT, MonadInterpreter,
                                                      errMsg)
import qualified Language.Haskell.Interpreter        as Interpreter
import           Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgsLibdir)
import           Playground.API                      (API, CompilationError, Evaluation, FunctionSchema,
                                                      FunctionSchema (FunctionSchema), SimpleArgumentSchema, SourceCode,
                                                      parseErrorText, toSimpleArgumentSchema)
import qualified Playground.Interpreter              as PI
import           Servant                             (err400, errBody, throwError)
import           Servant.API                         ((:<|>) ((:<|>)), NoContent (NoContent))
import           Servant.Server                      (Handler, Server)
import           System.Environment                  (lookupEnv)
import           Wallet.UTXO.Types                   (Blockchain)

newtype InterpreterInstance = InterpreterInstance (MVar ())
  deriving stock (Generic)

instance Newtype InterpreterInstance

mkInterpreterInstance :: IO InterpreterInstance
mkInterpreterInstance = InterpreterInstance <$> newMVar ()

runInterpreterInstance :: InterpreterInstance -> InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreterInstance i = withMVar (unpack i) . const . runInterpreter

runInterpreter :: InterpreterT IO a -> IO (Either InterpreterError a)
runInterpreter action = do
  mLibDir <- liftIO $ lookupEnv "GHC_LIB_DIR"
  case mLibDir of
    Just libDir -> unsafeRunInterpreterWithArgsLibdir [] libDir action
      -- TODO: We can make parsing easier by dumping json
      -- unsafeRunInterpreterWithArgsLibdir ["-ddump-json"] libDir action
    Nothing     -> Interpreter.runInterpreter action

acceptSourceCode ::
     InterpreterInstance
  -> SourceCode
  -> Handler (Either [CompilationError] [FunctionSchema SimpleArgumentSchema])
acceptSourceCode i sourceCode = do
  r <- liftIO . runInterpreterInstance i $ PI.compile sourceCode
  case r of
    Right vs -> pure . Right $ fmap toSimpleArgumentSchema <$> vs
    Left (WontCompile errors) ->
      pure $ Left $ map (parseErrorText . Text.pack . errMsg) errors
    Left e -> throwError $ err400 {errBody = BSL.pack . show $ e}

runFunction :: InterpreterInstance -> Evaluation -> Handler Blockchain
runFunction i e = do
  mLibDir <- liftIO $ lookupEnv "GHC_LIB_DIR"
  r <- liftIO $ runInterpreterInstance i $ PI.runFunction e
  case r of
    Left e           -> throwError $ err400 {errBody = BSL.pack . show $ e}
    Right blockchain -> pure blockchain

mkHandlers :: IO (Server API)
mkHandlers = do
  i <- mkInterpreterInstance
  pure $ acceptSourceCode i :<|> runFunction i
