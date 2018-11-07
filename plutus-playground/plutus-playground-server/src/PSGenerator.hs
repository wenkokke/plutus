{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module PSGenerator
  ( generate
  ) where

import           Control.Applicative                ((<|>))
import           Control.Lens                       (set, (&))
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Monoid                        ()
import           Data.Proxy                         (Proxy (Proxy))
import qualified Data.Set                           as Set ()
import           Data.Text                          (Text)
import qualified Data.Text                          as T ()
import qualified Data.Text.Encoding                 as T ()
import qualified Data.Text.IO                       as T ()
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge         (BridgePart, Language (Haskell), SumType, buildBridge, mkSumType,
                                                     stringBridge, writePSTypes)
import           Language.PureScript.Bridge.PSTypes ()
import           Playground.API                     (API, CompilationError, Evaluation, Fn, FunctionSchema,
                                                     FunctionsSchema, SourceCode)
import qualified Playground.API                     as API
import           Servant.API                        ((:>), Capture, Get, JSON, PlainText, Post, ReqBody)
import           Servant.PureScript                 (HasBridge, Settings, apiModuleName, defaultBridge, defaultSettings,
                                                     languageBridge, writeAPIModuleWithSettings, _generateSubscriberAPI)
import           Wallet.UTXO.Types                  (Blockchain)

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
psJson = TypeInfo "purescript-argonaut-core" "Data.Argonaut.Core" "Json" []

schemaBridge :: BridgePart
schemaBridge = do
  typeName ^== "Schema"
  typeModule ^== "Data.Swagger" <|> typeModule ^== "Data.Swagger.Internal"
  pure psSchema

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
  schemaBridge <|>
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

expressionType :: SumType 'Haskell
expressionType =
  let (SumType t d i) = mkSumType (Proxy @Expression)
   in SumType t d [SumType.Newtype]

evaluationType :: SumType 'Haskell
evaluationType =
  let (SumType t d i) = mkSumType (Proxy @Evaluation)
   in SumType t d [SumType.Newtype]

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy @FunctionSchema)
  , mkSumType (Proxy @Fn)
  , mkSumType (Proxy @SourceCode)
  , mkSumType (Proxy @Wallet)
  , mkSumType (Proxy @CompilationError)
  , evaluationType
  , expressionType
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

k =
  generate
    "/Users/davidsmith/tweag/plutus/plutus-playground/plutus-playground-client/src"

mySettings :: Settings
mySettings =
  (defaultSettings & set apiModuleName "Playground.Server")
    {_generateSubscriberAPI = False}

generate :: FilePath -> IO ()
generate outputDir = do
  writeAPIModuleWithSettings mySettings outputDir myBridgeProxy (Proxy @API.API)
  writePSTypes outputDir (buildBridge myBridge) myTypes
