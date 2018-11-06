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

import Control.Applicative ((<|>))
import Control.Lens ((&), set)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid ()
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import qualified Data.Set as Set ()
import Data.Swagger
  ( ExternalDocs
  , Format
  , ParamName
  , ParamSchema
  , Pattern
  , Reference
  , Referenced
  , Schema
  , SwaggerItems
  , SwaggerType
  , URL
  , Xml
  )
import Data.Swagger.Internal (SwaggerKind(SwaggerKindSchema))
import Data.Text (Text)
import qualified Data.Text as T ()
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import GHC.Generics (Generic)
import Language.PureScript.Bridge
  ( BridgeData
  , BridgePart
  , DataConstructor(DataConstructor)
  , HaskellType
  , Language(Haskell)
  , PSType
  , RecordEntry(RecordEntry)
  , SumType(SumType)
  , TypeInfo(TypeInfo)
  , (^==)
  , buildBridge
  , mkSumType
  , mkTypeInfo
  , psTypeParameters
  , stringBridge
  , typeModule
  , typeName
  , typeParameters
  , writePSTypes
  )
import qualified Language.PureScript.Bridge.SumType as SumType
import Language.PureScript.Bridge.PSTypes (psInt)
import Language.PureScript.Bridge.TypeParameters (A)
import Playground.API (API, Evaluation, Fn, FunctionSchema, SourceCode)
import qualified Playground.API as API
import Playground.Interpreter (CompilationError)
import Servant.API ((:>), Capture, Get, JSON, PlainText, Post, ReqBody)
import Servant.PureScript
  ( HasBridge
  , Settings
  , _generateSubscriberAPI
  , apiModuleName
  , defaultBridge
  , defaultSettings
  , languageBridge
  , writeAPIModuleWithSettings
  )
import Wallet.UTXO.Types (Blockchain)

myBridge :: BridgePart
myBridge =
  defaultBridge <|> integerBridge <|> scientificBridge <|> insOrdHashMapBridge <|>
  aesonBridge

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

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

referencedType :: SumType 'Haskell
referencedType =
  SumType
    info
    [ DataConstructor "Ref" (Left [referenceTypeInfo])
    , DataConstructor "Inline" (Left [mkTypeInfo (Proxy @A)])
    ]
    [SumType.Generic]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "Referenced" [mkTypeInfo (Proxy @A)]

referenceType :: SumType 'Haskell
referenceType =
  SumType
    referenceTypeInfo
    [ DataConstructor
        "Reference"
        (Right [RecordEntry "getReference" (mkTypeInfo (Proxy @Text))])
    ]
    [SumType.Newtype, SumType.Generic]

uRLType :: SumType 'Haskell
uRLType =
  SumType
    info
    [ DataConstructor
        "URL"
        (Right [RecordEntry "getUrl" (mkTypeInfo (Proxy @Text))])
    ]
    [SumType.Generic, SumType.Newtype]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "URL" []

swaggerKindType :: SumType 'Haskell
swaggerKindType =
  SumType
    info
    [ DataConstructor "SwaggerKindNormal" (Left [mkTypeInfo (Proxy @A)])
    , DataConstructor "SwaggerKindParamOtherSchema" (Left [])
    -- , DataConstructor "SwaggerKindSchema" (Left [])
    ]
    [SumType.Generic]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "SwaggerKind" [mkTypeInfo (Proxy @A)]

swaggerKindSchemaType :: SumType 'Haskell
swaggerKindSchemaType =
  SumType
    info
    [ DataConstructor "SwaggerKindSchema" (Left [])
    ]
    [SumType.Generic]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "SwaggerKindSchema" []

referenceTypeInfo :: TypeInfo 'Haskell
referenceTypeInfo = mkTypeInfo (Proxy @Reference)

schemaType :: SumType 'Haskell
schemaType =
  SumType
    info
    [ DataConstructor
        "Schema"
        (Right
           [ RecordEntry "_schemaTitle" (mkTypeInfo (Proxy @(Maybe Text)))
           , RecordEntry "_schemaDescription" (mkTypeInfo (Proxy @(Maybe Text)))
           , RecordEntry "_schemaRequired" (mkTypeInfo (Proxy @([ParamName])))
           , RecordEntry
               "_schemaAllOf"
               (mkTypeInfo (Proxy @(Maybe [Referenced Schema])))
           , RecordEntry
               "_schemaProperties"
               (mkTypeInfo (Proxy @(InsOrdHashMap Text (Referenced Schema))))
           , RecordEntry
               "_schemaAdditionalProperties"
               (mkTypeInfo (Proxy @(Maybe (Referenced Schema))))
           , RecordEntry
               "_schemaDiscriminator"
               (mkTypeInfo (Proxy @(Maybe Text)))
           , RecordEntry "_schemaReadOnly" (mkTypeInfo (Proxy @(Maybe Bool)))
           , RecordEntry "_schemaXml" (mkTypeInfo (Proxy @(Maybe Xml)))
           , RecordEntry
               "_schemaExternalDocs"
               (mkTypeInfo (Proxy @(Maybe ExternalDocs)))
           , RecordEntry "_schemaExample" (mkTypeInfo (Proxy @(Maybe Value)))
           , RecordEntry
               "_schemaMaxProperties"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_schemaMinProperties"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_schemaParamSchema"
               (mkTypeInfo (Proxy @(ParamSchema SwaggerKindSchema)))
           ])
    ]
    [SumType.Newtype, SumType.Generic]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "Schema" []

paramSchemaType :: SumType 'Haskell
paramSchemaType =
  SumType
    info
    [ DataConstructor
        "ParamSchema"
        (Right
           [ RecordEntry
               "_paramSchemaDefault"
               (mkTypeInfo (Proxy @(Maybe Value)))
           , RecordEntry "_paramSchemaType" (mkTypeInfo (Proxy @A))
           , RecordEntry
               "_paramSchemaFormat"
               (mkTypeInfo (Proxy @(Maybe Format)))
           , RecordEntry "_paramSchemaItems" (mkTypeInfo (Proxy @(Maybe A)))
           , RecordEntry
               "_paramSchemaMaximum"
               (mkTypeInfo (Proxy @(Maybe Scientific)))
           , RecordEntry
               "_paramSchemaExclusiveMaximum"
               (mkTypeInfo (Proxy @(Maybe Bool)))
           , RecordEntry
               "_paramSchemaMinimum"
               (mkTypeInfo (Proxy @(Maybe Scientific)))
           , RecordEntry
               "_paramSchemaExclusiveMinimum"
               (mkTypeInfo (Proxy @(Maybe Bool)))
           , RecordEntry
               "_paramSchemaMaxLength"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_paramSchemaMinLength"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_paramSchemaPattern"
               (mkTypeInfo (Proxy @(Maybe Pattern)))
           , RecordEntry
               "_paramSchemaMaxItems"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_paramSchemaMinItems"
               (mkTypeInfo (Proxy @(Maybe Integer)))
           , RecordEntry
               "_paramSchemaUniqueItems"
               (mkTypeInfo (Proxy @(Maybe Bool)))
           , RecordEntry
               "_paramSchemaEnum"
               (mkTypeInfo (Proxy @(Maybe [Value])))
           , RecordEntry
               "_paramSchemaMultipleOf"
               (mkTypeInfo (Proxy @(Maybe Scientific)))
           ])
    ]
    [SumType.Newtype, SumType.Generic]
  where
    info :: HaskellType
    info = TypeInfo "swagger2" "Data.Swagger.Internal" "ParamSchema" [mkTypeInfo (Proxy @A)]

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy @FunctionSchema)
  , mkSumType (Proxy @Fn)
  , mkSumType (Proxy @SourceCode)
  , mkSumType (Proxy @Xml)
  , mkSumType (Proxy @ExternalDocs)
  , mkSumType (Proxy @CompilationError)
  , swaggerKindType
  , swaggerKindSchemaType
  , uRLType
  , referenceType
  , referencedType
  , schemaType
  , paramSchemaType
  ]

mySettings :: Settings
mySettings =
  (defaultSettings & set apiModuleName "Playground.Server")
    {_generateSubscriberAPI = False}

generate :: FilePath -> IO ()
generate outputDir = do
  writeAPIModuleWithSettings
    mySettings
    outputDir
    myBridgeProxy
    (Proxy :: Proxy ("contract" :> ReqBody '[ JSON] SourceCode :> Post '[ JSON] [FunctionSchema]))
  writePSTypes outputDir (buildBridge myBridge) myTypes
