{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Playground.Interpreter where

import           Control.Monad.Catch          (finally, throwM)
import           Control.Monad.Error.Class    (MonadError, throwError)
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Newtype.Generics     as Newtype
import           Data.Aeson                   (FromJSON, Result (Success), ToJSON, encode, fromJSON)
import qualified Data.Aeson                   as JSON
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.List                    (intercalate)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromJust, fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Set                     as Set
import           Data.Swagger                 (Schema (Schema), ToSchema, declareNamedSchema, toSchema)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable                (TypeRep, Typeable, typeRepArgs)
import qualified Data.Typeable                as DT
import qualified Data.Typeable                as T
import           Debug.Trace                  (trace)
import           GHC.Generics                 (Generic)
import           Language.Haskell.Interpreter (Extension (..), GhcError, InterpreterError (UnknownError),
                                               ModuleElem (Fun), ModuleName, MonadInterpreter, OptionVal ((:=)), as,
                                               getLoadedModules, getModuleExports, interpret, languageExtensions,
                                               loadModules, runInterpreter, set, setImportsQ, setTopLevelModules,
                                               typeChecksWithDetails, typeOf)
import qualified Language.Haskell.TH          as TH
import           Playground.API               (Evaluation (program, sourceCode), Expression (Action, Wait), Fn (Fn),
                                               FunctionSchema (FunctionSchema),
                                               PlaygroundError (DecodeJsonTypeError, FunctionSchemaError), Program,
                                               SourceCode, arguments, blockchain, wallets)
import qualified Playground.TH                as TH
import           System.Directory             (removeFile)
import           System.IO                    (readFile)
import           System.IO.Temp               (writeSystemTempFile)
import qualified Type.Reflection              as TR
import           Wallet.API                   (WalletAPI, payToPubKey)
import           Wallet.Emulator.Types        (AssertionError, EmulatedWalletApi, EmulatorState (emChain), Trace,
                                               Wallet (Wallet), runTraceChain, runTraceTxPool, walletAction)
import           Wallet.Generators            (GeneratorModel (GeneratorModel))
import qualified Wallet.Generators            as Gen
import           Wallet.UTXO                  (Blockchain, Height, PubKey (PubKey), Tx, Value (Value))

$(TH.mkFunction 'payToPubKey)

defaultExtensions :: [Extension]
defaultExtensions =
  [ ExplicitForAll
  , ScopedTypeVariables
  , DeriveGeneric
  , StandaloneDeriving
  , DeriveLift
  , GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  ]

loadSource :: (MonadInterpreter m) => FilePath -> (ModuleName -> m a) -> m a
loadSource fileName action =
  flip finally (liftIO (removeFile fileName)) $ do
    set [languageExtensions := defaultExtensions]
    loadModules [fileName]
    (m:_) <- getLoadedModules
    setTopLevelModules [m]
    action m

compile :: (MonadInterpreter m, MonadError PlaygroundError m) => SourceCode -> m [FunctionSchema Schema]
compile s = do
  fileName <-
    liftIO $
    writeSystemTempFile "Contract.hs" (Text.unpack . Newtype.unpack $ s)
  loadSource fileName $ \moduleName -> do
    exports <- getModuleExports moduleName
    walletFunctions <- catMaybes <$> traverse isWalletFunction exports
    schemas <- traverse getSchema walletFunctions
    pure (schemas <> pure payToPubKeySchema)

jsonToString :: ToJSON a => a -> String
jsonToString = show . JSON.encode

{-# ANN getSchema ("HLint: ignore" :: String) #-}

getSchema :: (MonadInterpreter m, MonadError PlaygroundError m) => ModuleElem -> m (FunctionSchema Schema)
getSchema (Fun m) = interpret m (as :: FunctionSchema Schema)
getSchema _       = throwError FunctionSchemaError
  -- error "Trying to get a schema by calling something other than a function"

{-# ANN getJsonString ("HLint: ignore" :: String) #-}

getJsonString :: (MonadError PlaygroundError m) => JSON.Value -> m String
getJsonString (JSON.String s) = pure $ Text.unpack s
getJsonString v               = throwError . DecodeJsonTypeError "String" . BSL.unpack . encode $ v

runFunction :: (MonadInterpreter m, MonadError PlaygroundError m) => Evaluation -> m Blockchain
runFunction evaluation = do
  expr <- mkExpr evaluation
  fileName <-
    liftIO $
    writeSystemTempFile
      "Contract.hs"
      (Text.unpack . Newtype.unpack . sourceCode $ evaluation)
  loadSource fileName $ \_ -> do
    setImportsQ
      [("Playground.Interpreter.Util", Nothing), ("Wallet.Emulator", Nothing)]
    liftIO . putStrLn $ expr
    res <-
      interpret expr (as :: Either AssertionError Blockchain)
    case res of
      Left e           -> throwM . UnknownError $ show e
      Right blockchain -> pure blockchain

mkExpr :: (MonadError PlaygroundError m) => Evaluation -> m String
mkExpr evaluation = do
  let allWallets = fst <$> wallets evaluation
  exprs <- traverse (walletActionExpr allWallets) (program evaluation)
  pure $ "runTrace (decode " <> jsonToString (wallets evaluation) <> ") (" <>
      intercalate " >> " exprs <> ")"

walletActionExpr :: (MonadError PlaygroundError m) => [Wallet] -> Expression -> m String
walletActionExpr allWallets (Action (Fn f) wallet args) = do
  argStrings <- fmap show <$> traverse getJsonString args
  pure $ "(runWalletActionAndProcessPending (" <> show allWallets <> ") (" <> show wallet <> ") (" <> mkApplyExpr (Text.unpack f) argStrings <> "))"
walletActionExpr allWallets (Wait blocks) =
  pure $ "addBlocksAndNotify (" <> show allWallets <> ") " <> show blocks

mkApplyExpr :: String -> [String] -> String
mkApplyExpr functionName [] = functionName
mkApplyExpr functionName [a] = "apply1" <+> functionName <+> a
mkApplyExpr functionName [a, b] = "apply2" <+> functionName <+> a <+> b
mkApplyExpr functionName [a, b, c] = "apply3" <+> functionName <+> a <+> b <+> c
mkApplyExpr functionName [a, b, c, d] =
  "apply4" <+> functionName <+> a <+> b <+> c <+> d
mkApplyExpr functionName [a, b, c, d, e] =
  "apply5" <+> functionName <+> a <+> b <+> c <+> d <+> e
mkApplyExpr functionName [a, b, c, d, e, f] =
  "apply6" <+> functionName <+> a <+> b <+> c <+> d <+> e <+> f
mkApplyExpr functionName [a, b, c, d, e, f, g] =
  "apply7" <+> functionName <+> a <+> b <+> c <+> d <+> e <+> f <+> g

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b

isWalletFunction :: (MonadInterpreter m) => ModuleElem -> m (Maybe ModuleElem)
isWalletFunction f@(Fun s) = do
  t <- typeOf s
  pure $
    if t == "FunctionSchema Schema"
      then Just f
      else Nothing
isWalletFunction _ = pure Nothing
