module Marlowe.Blockly where

import Prelude

import Blockly (AlignDirection(..), Arg(..), BlockDefinition(..), block, blockType, category, colour, defaultBlockDefinition, getBlockById, initializeWorkspace, name, render, style, x, xml, y)
import Blockly.Generator (NewBlockFunction, Generator, Input, clearWorkspace, connectToOutput, connectToPrevious, fieldName, fieldRow, getFieldValue, getInputWithName, inputList, inputName, insertGeneratorFunction, mkGenerator, setFieldText, statementToCode)
import Blockly.Types (Block, BlocklyState, Workspace)
import Control.Alternative ((<|>))
import Control.Monad.ST as ST
import Control.Monad.ST.Internal (ST, STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either)
import Data.Enum (class BoundedEnum, class Enum, upFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Halogen.HTML (HTML)
import Halogen.HTML.Properties (id_)
import Marlowe.Parser as Parser
import Marlowe.Semantics (AccountId(..), ChoiceId(..), Contract(..), Observation(..), Value(..))
import Record (merge)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Basic (parens)

data ContractType
  = RefundContractType
  | PayContractType
  | IfContractType
  | WhenContractType
  | LetContractType

derive instance genericContractType :: Generic ContractType _

instance showContractType :: Show ContractType where
  show = genericShow

instance eqContractType :: Eq ContractType where
  eq = genericEq

instance ordContractType :: Ord ContractType where
  compare = genericCompare

instance enumContractType :: Enum ContractType where
  succ = genericSucc
  pred = genericPred

instance boundedContractType :: Bounded ContractType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumContractType :: BoundedEnum ContractType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

contractTypes :: Array ContractType
contractTypes = upFromIncluding bottom

data ObservationType
  = AndObservationType
  | OrObservationType
  | NotObservationType
  | ChoseSomethingObservationType
  | ValueGEObservationType
  | ValueGTObservationType
  | ValueLTObservationType
  | ValueLEObservationType
  | ValueEQObservationType
  | TrueObservationType
  | FalseObservationType

derive instance genericObservationType :: Generic ObservationType _

instance showObservationType :: Show ObservationType where
  show = genericShow

instance eqObservationType :: Eq ObservationType where
  eq = genericEq

instance ordObservationType :: Ord ObservationType where
  compare = genericCompare

instance enumObservationType :: Enum ObservationType where
  succ = genericSucc
  pred = genericPred

instance boundedObservationType :: Bounded ObservationType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumObservationType :: BoundedEnum ObservationType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

observationTypes :: Array ObservationType
observationTypes = upFromIncluding bottom

data ValueType
  = AvailableMoneyValueType
  | ConstantValueType
  | NegValueValueType
  | AddValueValueType
  | SubValueValueType
  | ChoiceValueValueType
  | SlotIntervalStartValueType
  | SlotIntervalEndValueType
  | UseValueValueType

derive instance genericValueType :: Generic ValueType _

instance showValueType :: Show ValueType where
  show = genericShow

instance eqValueType :: Eq ValueType where
  eq = genericEq

instance ordValueType :: Ord ValueType where
  compare = genericCompare

instance enumValueType :: Enum ValueType where
  succ = genericSucc
  pred = genericPred

instance boundedValueType :: Bounded ValueType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumValueType :: BoundedEnum ValueType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

valueTypes :: Array ValueType
valueTypes = upFromIncluding bottom

data BlockType
  = BaseContractType
  | ContractType ContractType
  | ObservationType ObservationType
  | ValueType ValueType

derive instance genericBlockType :: Generic BlockType _

instance eqBlockType :: Eq BlockType where
  eq = genericEq

instance ordBlockType :: Ord BlockType where
  compare = genericCompare

instance enumBlockType :: Enum BlockType where
  succ = genericSucc
  pred = genericPred

instance boundedBlockType :: Bounded BlockType where
  bottom = genericBottom
  top = genericTop

instance boundedEnumBlockType :: BoundedEnum BlockType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showBlockType :: Show BlockType where
  show BaseContractType = "BaseContractType"
  show (ContractType c) = show c
  show (ObservationType ot) = show ot
  show (ValueType vt) = show vt

blockDefinitions :: Array BlockDefinition
blockDefinitions = map toDefinition (upFromIncluding bottom)

toDefinition :: BlockType -> BlockDefinition
toDefinition BaseContractType =
  BlockDefinition
    $ merge
        { type: show BaseContractType
        , message0: "%1 CONTRACT %2 %3"
        , args0:
          [ DummyRight
          , Statement { name: (show BaseContractType), check: (show BaseContractType), align: Right }
          , DummyRight
          ]
        , colour: "0"
        , inputsInline: Just false
        }
        defaultBlockDefinition

-- Contracts

toDefinition (ContractType RefundContractType) =
  BlockDefinition
    $ merge
        { type: show RefundContractType
        , message0: "Refund"
        , colour: "0"
        , previousStatement: Just (show BaseContractType)
        }
        defaultBlockDefinition

toDefinition (ContractType PayContractType) =
  BlockDefinition
    $ merge
        { type: show PayContractType
        , message0: "Pay %1 person %2 %3 the amount of %4 %5 from the account %6 %7 with owner %8 %9 continue as %10"
        , args0:
          [ DummyCentre
          , Number { name: "payee_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , DummyRight
          , Number { name: "account_number", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , DummyRight
          , Value { name: "account_owner", check: "value", align: Right }
          , DummyRight
          , Value { name: "ammount", check: "value", align: Right }
          , Statement { name: "contract", check: (show BaseContractType), align: Right }
          ]
        , colour: "0"
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ContractType IfContractType) =
  BlockDefinition
    $ merge
        { type: show IfContractType
        , message0: "If observation %1 then continue as %2 otherwise continue as %3"
        , args0:
          [ Value { name: "observation", check: "observation", align: Right }
          , Statement { name: "contract1", check: (show BaseContractType), align: Right }
          , Statement { name: "contract2", check: (show BaseContractType), align: Right }
          ]
        , colour: "0"
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition
-- TODO: How do I do [Case]?
-- https://github.com/google/blockly/wiki/Lists
toDefinition (ContractType WhenContractType) =
  BlockDefinition
    $ merge
        { type: show WhenContractType
        , message0: "When observation %1 continue as %2 if block is %3 or higher continue as %4"
        , args0:
          [ Value { name: "observation", check: "observation", align: Right }
          , Statement { name: "contract1", check: (show BaseContractType), align: Right }
          , Number { name: "timeout", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing }
          , Statement { name: "contract2", check: (show BaseContractType), align: Right }
          ]
        , colour: "0"
        , previousStatement: Just (show BaseContractType)
        , inputsInline: Just false
        }
        defaultBlockDefinition

toDefinition (ContractType LetContractType) =
  BlockDefinition
    $ merge
        { type: show LetContractType
        , message0: "Let %1 be %2 continue as %3"
        , args0:
          [ Number { name: "value_id", value: 0.0, min: Just 0.0, max: Nothing, precision: Nothing }
          , Value { name: "value", check: "value", align: Right }
          , Statement { name: "contract", check: (show BaseContractType), align: Right }
          ]
        , colour: "0"
        , previousStatement: Just (show BaseContractType)
        }
        defaultBlockDefinition

-- Observations

toDefinition (ObservationType AndObservationType) =
  BlockDefinition
    $ merge
        { type: show AndObservationType
        , message0: "%1 and %2"
        , args0:
          [ Value { name: "observation1", check: "observation", align: Right }
          , Value { name: "observation2", check: "observation", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType OrObservationType) =
  BlockDefinition
    $ merge
        { type: show OrObservationType
        , message0: "%1 or %2"
        , args0:
          [ Value { name: "observation1", check: "observation", align: Right }
          , Value { name: "observation2", check: "observation", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType NotObservationType) =
  BlockDefinition
    $ merge
        { type: show NotObservationType
        , message0: "Not %1"
        , args0:
          [ Value { name: "observation", check: "observation", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ChoseSomethingObservationType) =
  BlockDefinition
    $ merge
        { type: show ChoseSomethingObservationType
        , message0: "chose id %1 for person %2"
        , args0:
          [ Number { name: "choice_number", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , Value { name: "choice_owner", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ValueGEObservationType) =
  BlockDefinition
    $ merge
        { type: show ValueGEObservationType
        , message0: "value %1 is greater than or equal to %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ValueGTObservationType) =
  BlockDefinition
    $ merge
        { type: show ValueGTObservationType
        , message0: "value %1 is greater than %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ValueLEObservationType) =
  BlockDefinition
    $ merge
        { type: show ValueLEObservationType
        , message0: "value %1 is less than or equal to %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ValueLTObservationType) =
  BlockDefinition
    $ merge
        { type: show ValueLTObservationType
        , message0: "value %1 is less than %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType ValueEQObservationType) =
  BlockDefinition
    $ merge
        { type: show ValueEQObservationType
        , message0: "value %1 is equal to %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType TrueObservationType) =
  BlockDefinition
    $ merge
        { type: show TrueObservationType
        , message0: "true"
        , lastDummyAlign0: Centre
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ObservationType FalseObservationType) =
  BlockDefinition
    $ merge
        { type: show FalseObservationType
        , message0: "false"
        , lastDummyAlign0: Centre
        , colour: "230"
        , output: Just "observation"
        , inputsInline: Just true
        }
        defaultBlockDefinition

-- Values

toDefinition (ValueType AvailableMoneyValueType) =
  BlockDefinition
    $ merge
        { type: show AvailableMoneyValueType
        , message0: "Available Money %1 from account number %2 %3 with owner %4 %5"
        , args0:
          [ DummyRight
          , Number { name: "account_number", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , DummyRight
          , Value { name: "account_owner", check: "value", align: Right }
          , DummyRight
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType ConstantValueType) =
  BlockDefinition
    $ merge
        { type: show ConstantValueType
        , message0: "Constant Value %1 ADA"
        , args0:
          [ Number { name: "constant", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType NegValueValueType) =
  BlockDefinition
    $ merge
        { type: show NegValueValueType
        , message0: "Negate Value %1"
        , args0:
          [ Value { name: "value", check: "value", align: Right }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType AddValueValueType) =
  BlockDefinition
    $ merge
        { type: show AddValueValueType
        , message0: "%1 + %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType SubValueValueType) =
  BlockDefinition
    $ merge
        { type: show SubValueValueType
        , message0: "%1 - %2"
        , args0:
          [ Value { name: "value1", check: "value", align: Right }
          , Value { name: "value2", check: "value", align: Right }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType ChoiceValueValueType) =
  BlockDefinition
    $ merge
        { type: show ChoiceValueValueType
        , message0: "use value of choice with id: %1 chosen by participant with id: %2 if no choice was made use: %3"
        , args0:
          [ Number { name: "choice_number", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , Number { name: "choice_owner", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          , Value { name: "value", check: "value", align: Right }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType SlotIntervalStartValueType) =
  BlockDefinition
    $ merge
        { type: show SlotIntervalStartValueType
        , message0: "Slot Interval Start"
        , lastDummyAlign0: Centre
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType SlotIntervalEndValueType) =
  BlockDefinition
    $ merge
        { type: show SlotIntervalEndValueType
        , message0: "Slot Interval End"
        , lastDummyAlign0: Centre
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition

toDefinition (ValueType UseValueValueType) =
  BlockDefinition
    $ merge
        { type: show UseValueValueType
        , message0: "Use Value with ID %1"
        , args0:
          [ Number { name: "value_id", value: 1.0, min: Just 1.0, max: Nothing, precision: Nothing }
          ]
        , colour: "135"
        , output: Just "value"
        , inputsInline: Just true
        }
        defaultBlockDefinition


toolbox :: forall a b. HTML a b
toolbox =
  xml [ id_ "blocklyToolbox", style "display:none" ]
    [ category [ name "Contracts", colour "0" ] (map mkBlock contractTypes)
    , category [ name "Observations", colour "230" ] (map mkBlock observationTypes)
    , category [ name "Values", colour "135" ] (map mkBlock valueTypes)
    ]
  where
  mkBlock :: forall t. Show t => t -> _
  mkBlock t = block [ blockType (show t) ] []

workspaceBlocks :: forall a b. HTML a b
workspaceBlocks =
  xml [ id_ "workspaceBlocks", style "display:none" ]
    [ block [ blockType (show BaseContractType), x "13", y "187", id_ "root_contract" ] []
    ]

parse :: forall a. Parser String a -> String -> Either String a
parse p = lmap show <<< flip runParser (parens p <|> p)

buildGenerator :: BlocklyState -> Generator
buildGenerator blocklyState =
  ST.run
    ( do
        gRef <- mkGenerator blocklyState "Marlowe"
        g <- STRef.read gRef
        traverse_ (\t -> mkGenFun gRef t (baseContractDefinition g)) [ BaseContractType ]
        traverse_ (\t -> mkGenFun gRef t (blockDefinition t g)) contractTypes
        traverse_ (\t -> mkGenFun gRef t (blockDefinition t g)) observationTypes
        traverse_ (\t -> mkGenFun gRef t (blockDefinition t g)) valueTypes
        STRef.read gRef
    )

mkGenFun :: forall a r t. Show a => Show t => STRef r Generator -> t -> (Block -> Either String a) -> ST r Unit
mkGenFun generator blockType f = insertGeneratorFunction generator (show blockType) ((rmap show) <<< f)

class HasBlockDefinition a b | a -> b where
  blockDefinition :: a -> Generator -> Block -> Either String b

baseContractDefinition :: Generator -> Block -> Either String Contract
baseContractDefinition g block = parse Parser.contract =<< statementToCode g block (show BaseContractType)

instance hasBlockDefinitionContract :: HasBlockDefinition ContractType Contract where
  blockDefinition RefundContractType _ _ = pure Refund
  blockDefinition PayContractType g block = do
    accountNumber <- parse Parser.bigInteger =<< getFieldValue block "account_number"
    accountOwner <- parse Parser.pubkey =<< getFieldValue block "account_owner"
    let
      accountId = AccountId { accountNumber, accountOwner }
    payee <- parse Parser.payee =<< getFieldValue block "payee_id"
    value <- parse Parser.value =<< statementToCode g block "value"
    contract <- parse Parser.contract =<< statementToCode g block "contract"
    pure (Pay accountId payee value contract)
  blockDefinition IfContractType g block = do
    observation <- parse Parser.observation =<< statementToCode g block "observation"
    contract1 <- parse Parser.contract =<< statementToCode g block "contract1"
    contract2 <- parse Parser.contract =<< statementToCode g block "contract2"
    pure (If observation contract1 contract2)
  blockDefinition WhenContractType g block = do
    cases <- parse Parser.cases =<< statementToCode g block "observation"
    timeout <- parse Parser.timeout =<< getFieldValue block "timeout"
    contract <- parse Parser.contract =<< statementToCode g block "contract"
    pure (When cases timeout contract)
  blockDefinition LetContractType g block = do
    valueId <- parse Parser.valueId =<< getFieldValue block "value_id"
    value <- parse Parser.value =<< statementToCode g block "value"
    contract <- parse Parser.contract =<< statementToCode g block "contract"
    pure (Let valueId value contract)

instance hasBlockDefinitionObservation :: HasBlockDefinition ObservationType Observation where
  blockDefinition AndObservationType g block = do
    observation1 <- parse Parser.observation =<< statementToCode g block "observation1"
    observation2 <- parse Parser.observation =<< statementToCode g block "observation2"
    pure (AndObs observation1 observation2)
  blockDefinition OrObservationType g block = do
    observation1 <- parse Parser.observation =<< statementToCode g block "observation1"
    observation2 <- parse Parser.observation =<< statementToCode g block "observation2"
    pure (OrObs observation1 observation2)
  blockDefinition NotObservationType g block = do
    observation <- parse Parser.observation =<< statementToCode g block "observation"
    pure (NotObs observation)
  blockDefinition ChoseSomethingObservationType g block = do
    choiceNumber <- parse Parser.bigInteger =<< getFieldValue block "choice_number"
    choiceOwner <- parse Parser.pubkey =<< getFieldValue block "choice_owner"
    let
      choiceId = ChoiceId { choiceNumber, choiceOwner }
    pure (ChoseSomething choiceId)
  blockDefinition ValueGEObservationType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (ValueGE value1 value2)
  blockDefinition ValueGTObservationType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (ValueGT value1 value2)
  blockDefinition ValueLEObservationType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (ValueLE value1 value2)
  blockDefinition ValueLTObservationType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (ValueLT value1 value2)
  blockDefinition ValueEQObservationType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (ValueEQ value1 value2)
  blockDefinition TrueObservationType g block = pure TrueObs
  blockDefinition FalseObservationType g block = pure FalseObs

instance hasBlockDefinitionValue :: HasBlockDefinition ValueType Value where
  blockDefinition AvailableMoneyValueType g block = do
    accountNumber <- parse Parser.bigInteger =<< getFieldValue block "account_number"
    accountOwner <- parse Parser.pubkey =<< getFieldValue block "account_owner"
    let
      accountId = AccountId { accountNumber, accountOwner }
    pure (AvailableMoney accountId)
  blockDefinition ConstantValueType g block = do
    constant <- parse Parser.bigInteger =<< getFieldValue block "constant"
    pure (Constant constant)
  blockDefinition NegValueValueType g block = do
    value <- parse Parser.value =<< statementToCode g block "value"
    pure (NegValue value)
  blockDefinition AddValueValueType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (AddValue value1 value2)
  blockDefinition SubValueValueType g block = do
    value1 <- parse Parser.value =<< statementToCode g block "value1"
    value2 <- parse Parser.value =<< statementToCode g block "value2"
    pure (SubValue value1 value2)
  blockDefinition ChoiceValueValueType g block = do
    choiceNumber <- parse Parser.bigInteger =<< getFieldValue block "choice_number"
    choiceOwner <- parse Parser.pubkey =<< getFieldValue block "choice_owner"
    let
      choiceId = ChoiceId { choiceNumber, choiceOwner }
    value <- parse Parser.value =<< statementToCode g block "value"
    pure (ChoiceValue choiceId value)
  blockDefinition SlotIntervalStartValueType g block = pure SlotIntervalStart
  blockDefinition SlotIntervalEndValueType g block = pure SlotIntervalEnd
  blockDefinition UseValueValueType g block = do
    valueId <- parse Parser.valueId =<< getFieldValue block "value_id"
    pure (UseValue valueId)

buildBlocks :: forall r. NewBlockFunction r -> BlocklyState -> Contract -> ST r Unit
buildBlocks newBlock bs contract = do
  workspaceRef <- STRef.new bs.workspace
  clearWorkspace workspaceRef
  initializeWorkspace bs.blockly workspaceRef
  let
    mContract = getBlockById bs.workspace "root_contract"
  rootBlock <- case mContract of
    Nothing -> do
      blockRef <- newBlock workspaceRef (show BaseContractType)
      STRef.read blockRef
    Just block -> pure block
  let
    inputs = inputList rootBlock

    mInput = getInputWithName inputs (show BaseContractType)
  case mInput of
    Nothing -> pure unit
    Just i -> do
      toBlockly newBlock workspaceRef i contract
      render workspaceRef

setField :: forall r. (STRef r Block) -> String -> String -> ST r Unit
setField blockRef name value = do
  block <- STRef.read blockRef
  let
    fields = inputList block >>= fieldRow
  case Array.find (\f -> fieldName f == name) fields of
    Nothing -> pure unit
    Just f -> do
      field <- STRef.new f
      setFieldText field value

inputToBlockly :: forall a r. ToBlockly a => NewBlockFunction r -> (STRef r Workspace) -> STRef r Block -> String -> a -> ST r Unit
inputToBlockly newBlock workspaceRef blockRef name value = do
  block <- STRef.read blockRef
  case Array.find (\i -> inputName i == name) (inputList block) of
    Nothing -> pure unit
    Just input -> toBlockly newBlock workspaceRef input value

class ToBlockly a where
  toBlockly :: forall r. NewBlockFunction r -> STRef r Workspace -> Input -> a -> ST r Unit

instance toBlocklyContract :: ToBlockly Contract where
  toBlockly newBlock workspace input Refund = do
    block <- newBlock workspace (show RefundContractType)
    connectToPrevious block input
  toBlockly newBlock workspace input (Pay accountId payee value contract) = do
    block <- newBlock workspace (show PayContractType)
    connectToPrevious block input
    setField block "account_number" (show (unwrap accountId).accountNumber)
    setField block "account_owner" (show (unwrap accountId).accountOwner)
    setField block "payee_id" (show payee)
    inputToBlockly newBlock workspace block "value" value
    inputToBlockly newBlock workspace block "contract" contract
  toBlockly newBlock workspace input (If observation contract1 contract2) = do
    block <- newBlock workspace (show IfContractType)
    connectToPrevious block input
    inputToBlockly newBlock workspace block "observation" observation
    inputToBlockly newBlock workspace block "contract1" contract1
    inputToBlockly newBlock workspace block "contract2" contract2
  toBlockly newBlock workspace input (When cases timeout contract) = do
    block <- newBlock workspace (show WhenContractType)
    connectToPrevious block input
    -- TODO: cases
    -- inputToBlockly newBlock workspace block "cases" cases
    setField block "timeout" (show timeout)
    inputToBlockly newBlock workspace block "contract" contract
  toBlockly newBlock workspace input (Let valueId value contract) = do
    block <- newBlock workspace (show LetContractType)
    connectToPrevious block input
    setField block "value_id" (show valueId)
    inputToBlockly newBlock workspace block "value" value
    inputToBlockly newBlock workspace block "contract" contract

instance toBlocklyObservation :: ToBlockly Observation where
  toBlockly newBlock workspace input (AndObs observation1 observation2) = do
    block <- newBlock workspace (show AndObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation1" observation1
    inputToBlockly newBlock workspace block "observation2" observation2
  toBlockly newBlock workspace input (OrObs observation1 observation2) = do
    block <- newBlock workspace (show OrObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation1" observation1
    inputToBlockly newBlock workspace block "observation2" observation2
  toBlockly newBlock workspace input (NotObs observation) = do
    block <- newBlock workspace (show NotObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "observation" observation
  toBlockly newBlock workspace input (ChoseSomething choiceId) = do
    block <- newBlock workspace (show ChoseSomethingObservationType)
    connectToOutput block input
    setField block "choice_number" (show (unwrap choiceId).choiceNumber)
    setField block "choice_owner" (show (unwrap choiceId).choiceOwner)
  toBlockly newBlock workspace input (ValueGE v1 v2) = do
    block <- newBlock workspace (show ValueGEObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueGT v1 v2) = do
    block <- newBlock workspace (show ValueGTObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueLT v1 v2) = do
    block <- newBlock workspace (show ValueLTObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueLE v1 v2) = do
    block <- newBlock workspace (show ValueLEObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ValueEQ v1 v2) = do
    block <- newBlock workspace (show ValueEQObservationType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input TrueObs = do
    block <- newBlock workspace (show TrueObservationType)
    connectToOutput block input
  toBlockly newBlock workspace input FalseObs = do
    block <- newBlock workspace (show FalseObservationType)
    connectToOutput block input

instance toBlocklyValue :: ToBlockly Value where
  toBlockly newBlock workspace input (AvailableMoney accountId) = do
    block <- newBlock workspace (show AvailableMoneyValueType)
    connectToOutput block input
    setField block "account_number" (show (unwrap accountId).accountNumber)
    setField block "account_owner" (show (unwrap accountId).accountOwner)
  toBlockly newBlock workspace input (Constant v) = do
    block <- newBlock workspace (show ConstantValueType)
    connectToOutput block input
    setField block "constant" (show v)
  toBlockly newBlock workspace input (NegValue v) = do
    block <- newBlock workspace (show NegValueValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value" v
  toBlockly newBlock workspace input (AddValue v1 v2) = do
    block <- newBlock workspace (show AddValueValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (SubValue v1 v2) = do
    block <- newBlock workspace (show SubValueValueType)
    connectToOutput block input
    inputToBlockly newBlock workspace block "value1" v1
    inputToBlockly newBlock workspace block "value2" v2
  toBlockly newBlock workspace input (ChoiceValue choiceId value) = do
    block <- newBlock workspace (show ChoiceValueValueType)
    connectToOutput block input
    setField block "choice_number" (show (unwrap choiceId).choiceNumber)
    setField block "choice_owner" (show (unwrap choiceId).choiceOwner)
    inputToBlockly newBlock workspace block "value" value
  toBlockly newBlock workspace input SlotIntervalStart = do
    block <- newBlock workspace (show SlotIntervalStartValueType)
    connectToOutput block input
  toBlockly newBlock workspace input SlotIntervalEnd = do
    block <- newBlock workspace (show SlotIntervalEndValueType)
    connectToOutput block input
  toBlockly newBlock workspace input (UseValue valueId) = do
    block <- newBlock workspace (show UseValueValueType)
    connectToOutput block input
    setField block "value_id" (show (unwrap valueId))
