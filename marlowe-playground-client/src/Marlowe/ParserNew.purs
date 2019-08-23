module Marlowe.ParserNew where


import Prelude ((*>), (<*), (<*>), bind, pure, (<$>), void, ($), (<<<), discard)

import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.List (List, many, some)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String.CodeUnits (fromCharArray)
import Marlowe.SemanticsNew (AccountId, Action(..), Bound, Case, ChoiceId, Contract(..), Observation(..), Party, Payee(..), PubKey, Slot(..), Timeout, Value(..), ValueId(..))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Basic (integral, parens)
import Text.Parsing.Parser.Combinators (between, choice)
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (alphaNum, space)

-- All arguments are space separated so we add **> to reduce boilerplate
maybeSpaces :: Parser String (List Char)
maybeSpaces = many space

spaces :: Parser String (List Char)
spaces = some space

appRSpaces :: forall a b. Parser String a -> Parser String b -> Parser String b
appRSpaces p q = p *> spaces *> q

infixl 4 appRSpaces as **>

appSpaces :: forall a b. Parser String (a -> b) -> Parser String a -> Parser String b
appSpaces p q = p <*> (spaces *> q)

infixl 4 appSpaces as <**>

text :: Parser String String
text = between (char '"') (char '"') $ fromCharArray <<< fromFoldable <$> many (choice [alphaNum, space])

----------------------------------------------------------------------
bigInteger :: Parser String BigInteger
bigInteger = do
  i <- integral
  case BigInteger.fromString i of
    (Just v) -> pure v
    Nothing -> fail "not a valid BigInt"

valueId :: Parser String ValueId
valueId = ValueId <$> bigInteger

slot :: Parser String Slot
slot = Slot <$> bigInteger

timeout :: Parser String Timeout
timeout = slot

accountId :: Parser String AccountId
accountId =
  parens do
    void maybeSpaces
    void $ string "AccountId"
    void spaces
    first <- bigInteger
    void spaces
    second <- text
    void maybeSpaces
    pure $ wrap { accountNumber: first, accountOwner: second }

choiceId :: Parser String ChoiceId
choiceId =
  parens do
    void maybeSpaces
    void $ string "ChoiceId"
    void spaces
    first <- bigInteger
    void spaces
    second <- text
    void maybeSpaces
    pure $ wrap { choiceNumber: first, choiceOwner: second }

atomValue :: Parser String Value
atomValue = pure SlotIntervalStart <* string "SlotIntervalStart"
    <|> pure SlotIntervalEnd <* string "SlotIntervalEnd"

recValue :: Parser String Value
recValue =
  (AvailableMoney <$> (string "AvailableMoney" **> accountId))
    <|> (Constant <$> (string "Constant" **> bigInteger))
    <|> (NegValue <$> (string "NegValue" **> value'))
    <|> (AddValue <$> (string "AddValue" **> value') <**> value')
    <|> (SubValue <$> (string "SubValue" **> value') <**> value')
    <|> (ChoiceValue <$> (string "ChoiceValue" **> choiceId) <**> value')
    <|> (UseValue <$> (string "UseValue" **> valueId))
  where
  value' :: Parser String Value
  value' = atomValue <|> fix (\p -> parens recValue)

value :: Parser String Value
value = atomValue <|> recValue


atomObservation :: Parser String Observation
atomObservation =
  pure TrueObs <* string "TrueObs"
    <|> pure FalseObs
    <* string "FalseObs"

recObservation :: Parser String Observation
recObservation =
    (AndObs <$> (string "AndObs" **> observation') <**> observation')
    <|> (OrObs <$> (string "OrObs" **> observation') <**> observation')
    <|> (NotObs <$> (string "NotObs" **> observation'))
    <|> (ChoseSomething <$> (string "ChoseSomething" **> choiceId))
    <|> (ValueGE <$> (string "ValueGE" **> value') <**> value')
    <|> (ValueGT <$> (string "ValueGT" **> value') <**> value')
    <|> (ValueLT <$> (string "ValueLT" **> value') <**> value')
    <|> (ValueLE <$> (string "ValueLE" **> value') <**> value')
    <|> (ValueEQ <$> (string "ValueEQ" **> value') <**> value')
  where
  observation' = atomObservation <|> fix \p -> parens recObservation

  value' = atomValue <|> fix (\p -> parens value)

observation :: Parser String Observation
observation = atomObservation <|> recObservation

payee :: Parser String Payee
payee = (Account <$> (string "Account" **> accountId))
        <|> (Party <$> (string "Party" **> text))

pubkey :: Parser String PubKey
pubkey = text

party :: Parser String Party
party = pubkey

bound :: Parser String Bound
bound =
  parens do
    void maybeSpaces
    void $ string "Bound"
    void spaces
    first <- bigInteger
    void spaces
    second <- bigInteger
    void maybeSpaces
    pure $ wrap { from: first, to: second }

action :: Parser String Action
action =
    (Deposit <$> (string "Deposit" **> accountId) <**> party <**> value)
    <|> (Choice <$> (string "Choice" **> choiceId) <**> many bound)
    <|> (Notify <$> (string "Notify" **> observation))

case' :: Parser String Case
case' = 
  parens do
    void maybeSpaces
    void $ string "Case"
    void spaces
    first <- action
    void spaces
    second <- contract
    void maybeSpaces
    pure $ wrap { action: first, contract: second }

atomContract :: Parser String Contract
atomContract = pure Refund <* string "Refund"

recContract :: Parser String Contract
recContract =
    ( Pay <$> (string "Pay" **> accountId)
          <**> payee
          <**> value'
          <**> contract'
      )
    <|> (If <$> (string "If" **> observation') <**> contract' <**> contract')
    <|> (When <$> (string "When" **> (many case')) <**> timeout <**> contract')
    <|> (Let <$> (string "Let" **> valueId) <**> value <**> contract')
    <|> (fail "not a valid Contract")
  where
  contract' = atomContract <|> fix \p -> parens recContract

  observation' = atomObservation <|> fix \p -> parens observation

  value' = atomValue <|> fix (\p -> parens value)

contract :: Parser String Contract
contract = do
  void maybeSpaces
  c <- (atomContract <|> recContract)
  void maybeSpaces
  pure c
