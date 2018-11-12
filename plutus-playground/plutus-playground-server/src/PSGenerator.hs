{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module PSGenerator
  ( generate
  ) where

import           Control.Applicative                       ((<|>))
import           Control.Lens                              (iso, set, view, (&))
import           Control.Monad.Reader.Class                (MonadReader)
import           Crypto.Hash                               (Digest, SHA256)
import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.HashMap.Strict.InsOrd                (InsOrdHashMap)
import           Data.Monoid                               ()
import           Data.Proxy                                (Proxy (Proxy))
import           Data.Scientific                           (Scientific)
import qualified Data.Set                                  as Set ()
import           Data.Set.Internal                         (Set)
import           Data.Swagger                              (ExternalDocs, Format, ParamName, ParamSchema, Pattern,
                                                            Reference, Referenced, Schema, SwaggerItems, SwaggerType,
                                                            URL, Xml)
import           Data.Swagger.Internal                     (SwaggerKind (SwaggerKindSchema))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T ()
import qualified Data.Text.Encoding                        as T ()
import qualified Data.Text.IO                              as T ()
import           GHC.Generics                              (Generic)
import           Language.PureScript.Bridge                (BridgeData, BridgePart, DataConstructor (DataConstructor),
                                                            HaskellType, Language (Haskell), PSType,
                                                            RecordEntry (RecordEntry), SumType (SumType),
                                                            TypeInfo (TypeInfo), buildBridge, doCheck, haskType,
                                                            mkSumType, mkTypeInfo, psTypeParameters, stringBridge,
                                                            typeModule, typeName, typeParameters, writePSTypes, (^==),
                                                            _typeModule, _typeName, _typePackage, _typeParameters)
import           Language.PureScript.Bridge.PSTypes        (psArray, psInt, psString)
import qualified Language.PureScript.Bridge.SumType        as SumType
import           Language.PureScript.Bridge.TypeParameters (A)
import           Playground.API                            (API, CompilationError, Evaluation, Expression, Fn,
                                                            FunctionSchema, SimpleArgumentSchema, SourceCode)
import qualified Playground.API                            as API
import           Servant.API                               ((:>), Capture, Get, JSON, PlainText, Post, ReqBody)
import           Servant.PureScript                        (HasBridge, Settings, apiModuleName, defaultBridge,
                                                            defaultSettings, languageBridge, writeAPIModuleWithSettings,
                                                            _generateSubscriberAPI)
import           Wallet.Emulator.Types                     (Wallet)
import           Wallet.UTXO.Types                         (Address, Blockchain, PubKey, Redeemer, Signature, Tx, TxId,
                                                            TxIn, TxInType, TxOut, TxOutRef, TxOutType, Validator,
                                                            Value)

integerBridge :: BridgePart
integerBridge = do
  typeName ^== "Integer"
  pure psInt

scientificBridge :: BridgePart
scientificBridge = do
  typeName ^== "Scientific"
  typeModule ^== "Data.Scientific"
  pure psInt

insOrdHashMapBridge :: BridgePart
insOrdHashMapBridge = do
  typeName ^== "InsOrdHashMap"
  psMap

psMap :: MonadReader BridgeData m => m PSType
psMap = TypeInfo "purescript-maps" "Data.Map" "Map" <$> psTypeParameters

aesonBridge :: BridgePart
aesonBridge = do
  typeName ^== "Value"
  typeModule ^== "Data.Aeson.Types.Internal"
  pure psJson

psJson :: PSType
psJson = TypeInfo "" "Data.RawJson" "RawJson" []

psSchema :: PSType
psSchema = TypeInfo "" "Data.JsonSchema" "Schema" []

setBridge :: BridgePart
setBridge = do
  typeName ^== "Set"
  typeModule ^== "Data.Set" <|> typeModule ^== "Data.Set.Internal"
  psArray

psSet :: MonadReader BridgeData m => m PSType
psSet =
  TypeInfo "purescript-ordered-collections" "Data.Set" "Set" <$>
  psTypeParameters

digestBridge :: BridgePart
digestBridge = do
  typeName ^== "Digest"
  typeModule ^== "Crypto.Hash.Types"
  pure psString

sha256Bridge :: BridgePart
sha256Bridge = do
  typeName ^== "SHA256"
  typeModule ^== "Crypto.Hash.Types" <|> typeModule ^== "Crypto.Hash" <|>
    typeModule ^== "Crypto.Hash.SHA256"
  pure psString

redeemerBridge :: BridgePart
redeemerBridge = do
  typeName ^== "Redeemer"
  typeModule ^== "Wallet.UTXO.Types"
  pure psString

validatorBridge :: BridgePart
validatorBridge = do
  typeName ^== "Validator"
  typeModule ^== "Wallet.UTXO.Types"
  pure psString

dataScriptBridge :: BridgePart
dataScriptBridge = do
  typeName ^== "DataScript"
  typeModule ^== "Wallet.UTXO.Types"
  pure psString

myBridge :: BridgePart
myBridge =
  defaultBridge <|> integerBridge <|> scientificBridge <|> insOrdHashMapBridge <|>
  aesonBridge <|>
  setBridge <|>
  digestBridge <|>
  sha256Bridge <|>
  redeemerBridge <|>
  validatorBridge <|>
  dataScriptBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy @SimpleArgumentSchema)
  , mkSumType (Proxy @(FunctionSchema A))
  , mkSumType (Proxy @Fn)
  , mkSumType (Proxy @SourceCode)
  , mkSumType (Proxy @Wallet)
  , mkSumType (Proxy @CompilationError)
  , mkSumType (Proxy @Expression)
  , mkSumType (Proxy @Evaluation)
  , mkSumType (Proxy @Tx)
  , mkSumType (Proxy @(TxIn A))
  , mkSumType (Proxy @(TxOutRef A))
  , mkSumType (Proxy @TxOutType)
  , mkSumType (Proxy @(TxOut A))
  , mkSumType (Proxy @(TxId A))
  , mkSumType (Proxy @TxInType)
  , mkSumType (Proxy @Signature)
  , mkSumType (Proxy @Value)
  , mkSumType (Proxy @PubKey)
  , mkSumType (Proxy @(Address A))
  ]

mySettings :: Settings
mySettings =
  (defaultSettings & set apiModuleName "Playground.Server")
    {_generateSubscriberAPI = False}

generate :: FilePath -> IO ()
generate outputDir = do
  writeAPIModuleWithSettings mySettings outputDir myBridgeProxy (Proxy @API.API)
  writePSTypes outputDir (buildBridge myBridge) myTypes


k = generate "/Users/kris/Work/Clients/IOHK/plutus/plutus-playground/plutus-playground-client/src"
