{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Playground.Interpreter where

import           Control.Monad.Catch          (finally, throwM)
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Newtype.Generics     as Newtype
import           Data.Aeson                   (FromJSON, Result (Success), ToJSON, fromJSON)
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
import           Playground.API               (Evaluation (program, sourceCode), Expression (Expression), Fn (Fn),
                                               FunctionSchema (FunctionSchema), Program, SourceCode, arguments,
                                               blockchain, wallets)
import qualified Playground.TH                as TH
import           System.Directory             (removeFile)
import           System.IO                    (readFile)
import           System.IO.Temp               (writeSystemTempFile)
import qualified Type.Reflection              as TR
import           Wallet.API                   (WalletAPI)
import           Wallet.Emulator.Types        (AssertionError, EmulatedWalletApi, EmulatorState (emChain), Trace,
                                               Wallet (Wallet), runTraceChain, runTraceTxPool, walletAction)
import           Wallet.Generators            (GeneratorModel (GeneratorModel))
import qualified Wallet.Generators            as Gen
import           Wallet.UTXO                  (Tx, Blockchain, Height, PubKey (PubKey), Value (Value))

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

compile :: (MonadInterpreter m) => SourceCode -> m [FunctionSchema Schema]
compile s = do
  fileName <-
    liftIO $
    writeSystemTempFile "Contract.hs" (Text.unpack . Newtype.unpack $ s)
  loadSource fileName $ \moduleName -> do
    exports <- getModuleExports moduleName
    walletFunctions <- catMaybes <$> traverse isWalletFunction exports
    traverse getSchema walletFunctions

{-# ANN getSchema ("HLint: ignore" :: String) #-}

getSchema :: (MonadInterpreter m) => ModuleElem -> m (FunctionSchema Schema)
getSchema (Fun m) = interpret m (as :: FunctionSchema Schema)
getSchema _ =
  error "Trying to get a schema by calling something other than a function"

getJsonString :: JSON.Value -> String
getJsonString (JSON.String s) = Text.unpack s
getJsonString _               = error "failed to decode as a JSON String"

runFunction :: (MonadInterpreter m) => Evaluation -> m Blockchain
runFunction evaluation = do
  liftIO . print . arguments . head . program $ evaluation
  fileName <-
    liftIO $
    writeSystemTempFile
      "Contract.hs"
      (Text.unpack . Newtype.unpack . sourceCode $ evaluation)
  loadSource fileName $ \_ -> do
    setImportsQ
      [("Playground.Interpreter", Nothing), ("Wallet.Emulator", Nothing)]
    liftIO . putStrLn $ mkExpr evaluation
    res <-
      interpret (mkExpr evaluation) (as :: Either AssertionError Blockchain)
    case res of
      Left e           -> throwM . UnknownError $ show e
      Right blockchain -> pure blockchain

runTrace ::
     [(Wallet, Integer)]
  -> Trace EmulatedWalletApi [Tx]
  -> Either AssertionError Blockchain
runTrace wallets action =
  let walletToBalance (Wallet i, v) = (PubKey i, Value v)
      initialBalance = Map.fromList $ fmap walletToBalance wallets
      pubKeys = Set.fromList $ fmap (\(Wallet i, _) -> PubKey i) wallets
      (initialTx, _) =
        Gen.genInitialTransaction $ GeneratorModel initialBalance pubKeys
      (eRes, newState) = runTraceTxPool [initialTx] action
   in case eRes of
        Right _ -> Right . emChain $ newState
        Left e  -> Left e

mkExpr :: Evaluation -> String
mkExpr evaluation =
  let allWallets = fst <$> wallets evaluation
   in "runTrace (decode " <> jsonToString (wallets evaluation) <> ") (" <>
      (intercalate " >> " $
       fmap (walletActionExpr allWallets) (program evaluation)) <>
      ")"

walletActionExpr :: [Wallet] -> Expression -> String
walletActionExpr allWallets (Expression (Fn f) wallet args) =
  "(runWalletActionAndProcessPending (" <> show allWallets <> ") (" <>
  show wallet <>
  ") (" <>
  mkApplyExpr (Text.unpack f) (fmap (show . getJsonString) args) <>
  "))"

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

jsonToString :: ToJSON a => a -> String
jsonToString = show . JSON.encode

-- | This will throw an exception if it cannot decode the json however it should
--   never do this as long as it is only called in places where we have already
--   decoded and encoded the value since it came from an HTTP API call
{-# ANN decode ("HLint: ignore" :: String) #-}

decode :: (FromJSON a, T.Typeable a) => String -> a
decode v =
  let x = JSON.eitherDecode . BSL.pack $ v
   in case x of
        Right a -> a
        Left e ->
          error $
          "couldn't decode " ++
          v ++ " :: " ++ show (T.typeOf x) ++ " (" ++ show e ++ ")"

apply1 :: (T.Typeable a, FromJSON a) => (a -> b) -> String -> b
apply1 fun v = fun (decode v)

apply2 :: (T.Typeable a, FromJSON a, T.Typeable b, FromJSON b) => (a -> b -> c) -> String -> String -> c
apply2 fun a b = fun (decode a) (decode b)

apply3 ::
     (T.Typeable a, T.Typeable a, FromJSON a, T.Typeable b, FromJSON b, T.Typeable c, FromJSON c)
  => (a -> b -> c -> d)
  -> String
  -> String
  -> String
  -> d
apply3 fun a b c = fun (decode a) (decode b) (decode c)

apply4 ::
     (T.Typeable a, FromJSON a, T.Typeable b, FromJSON b, T.Typeable c, FromJSON c, T.Typeable d, FromJSON d)
  => (a -> b -> c -> d -> e)
  -> String
  -> String
  -> String
  -> String
  -> e
apply4 fun a b c d = fun (decode a) (decode b) (decode c) (decode d)

apply5 ::
     (T.Typeable a, FromJSON a, T.Typeable b, FromJSON b, T.Typeable c, FromJSON c, T.Typeable d, FromJSON d, T.Typeable e, FromJSON e)
  => (a -> b -> c -> d -> e -> f)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f
apply5 fun a b c d e =
  fun (decode a) (decode b) (decode c) (decode d) (decode e)

apply6 ::
     (T.Typeable a, FromJSON a, T.Typeable b, FromJSON b, T.Typeable c, FromJSON c, T.Typeable d, FromJSON d, T.Typeable e, FromJSON e, T.Typeable f, FromJSON f)
  => (a -> b -> c -> d -> e -> f -> g)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> g
apply6 fun a b c d e f =
  fun (decode a) (decode b) (decode c) (decode d) (decode e) (decode f)

apply7 ::
     ( T.Typeable a, FromJSON a
     , T.Typeable b, FromJSON b
     , T.Typeable c, FromJSON c
     , T.Typeable d, FromJSON d
     , T.Typeable e, FromJSON e
     , T.Typeable f, FromJSON f
     , T.Typeable g, FromJSON g
     )
  => (a -> b -> c -> d -> e -> f -> g -> h)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> h
apply7 fun a b c d e f g =
  fun
    (decode a)
    (decode b)
    (decode c)
    (decode d)
    (decode e)
    (decode f)
    (decode g)

isWalletFunction :: (MonadInterpreter m) => ModuleElem -> m (Maybe ModuleElem)
isWalletFunction f@(Fun s) = do
  t <- typeOf s
  pure $
    if t == "FunctionSchema Schema"
      then Just f
      else Nothing
isWalletFunction _ = pure Nothing
