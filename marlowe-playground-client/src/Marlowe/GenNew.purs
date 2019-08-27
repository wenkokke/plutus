module Marlowe.GenNew where

import Prelude
import Control.Lazy (class Lazy, defer)
import Control.Monad.Gen (class MonadGen, chooseInt, resize, suchThat, unfoldable)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.Foldable (class Foldable)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, foldl1, (:|))
import Data.String.CodeUnits (fromCharArray)
import Marlowe.SemanticsNew (AccountId, Action(..), Bound, Case, ChoiceId, Contract(..), Interval, Observation(..), Payee(..), PubKey, Slot(..), Timeout, Value(..), ValueId(..))

oneOf ::
  forall m a f.
  Foldable f =>
  MonadGen m =>
  NonEmpty f (m a) ->
  m a
oneOf = foldl1 Gen.choose

genBigInteger :: forall m. MonadGen m => MonadRec m => m BigInteger
genBigInteger = BigInteger.fromInt <$> chooseInt bottom top

genSlot :: forall m. MonadGen m => MonadRec m => m Slot
genSlot = Slot <$> genBigInteger

genTimeout :: forall m. MonadGen m => MonadRec m => m Timeout
genTimeout = genSlot

genValueId :: forall m. MonadGen m => MonadRec m => m ValueId
genValueId = ValueId <$> genBigInteger

genAlphaNum :: forall m. MonadGen m => MonadRec m => m Char
genAlphaNum = oneOf $ genAlpha :| [ genDigitChar ]

genPubKey :: forall m. MonadGen m => MonadRec m => m PubKey
genPubKey = fromCharArray <$> resize (_ - 1) (unfoldable genAlphaNum)

genInterval :: forall m a. MonadGen m => MonadRec m => Ord a => m a -> m (Interval a)
genInterval gen = do
  from <- gen
  to <- suchThat gen (\v -> v > from)
  pure $ wrap { from, to }

genBound :: forall m. MonadGen m => MonadRec m => m Bound
genBound = genInterval genBigInteger

genAccountId :: forall m. MonadGen m => MonadRec m => m AccountId
genAccountId = do
  accountNumber <- genBigInteger
  accountOwner <- genPubKey
  pure $ wrap { accountNumber, accountOwner }

genChoiceId :: forall m. MonadGen m => MonadRec m => m ChoiceId
genChoiceId = do
  choiceNumber <- genBigInteger
  choiceOwner <- genPubKey
  pure $ wrap { choiceNumber, choiceOwner }

genPayee :: forall m. MonadGen m => MonadRec m => m Payee
genPayee = oneOf $ (Account <$> genAccountId) :| [ Party <$> genPubKey ]

genAction :: forall m. MonadGen m => MonadRec m => Lazy (m Observation) => Lazy (m Value) => Int -> m Action
genAction size =
  oneOf
    $ (Deposit <$> genAccountId <*> genPubKey <*> genValue' size)
    :| [ Choice <$> genChoiceId <*> resize (_ - 1) (unfoldable genBound)
      , Notify <$> genObservation' size
      ]

genCase :: forall m. MonadGen m => MonadRec m => Lazy (m Value) => Lazy (m Observation) => Lazy (m Contract) => Int -> m Case
genCase size = do
  let
    newSize = size - 1
  action <- genAction newSize
  contract <- genContract' newSize
  pure $ wrap { action, contract }

genCases :: forall m. MonadGen m => MonadRec m => Lazy (m Value) => Lazy (m Observation) => Lazy (m Contract) => Int -> m (Array Case)
genCases size = resize (_ - 1) (unfoldable (genCase size))

genValue :: forall m. MonadGen m => MonadRec m => Lazy (m Value) => m Value
genValue = genValue' 5

genValue' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Value) =>
  Int ->
  m Value
genValue' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genValue' newSize
      in
        oneOf $ pure SlotIntervalStart
          :| [ pure SlotIntervalEnd
            , AvailableMoney <$> genAccountId
            , Constant <$> genBigInteger
            , NegValue <$> genNewValue
            , AddValue <$> genNewValue <*> genNewValue
            , SubValue <$> genNewValue <*> genNewValue
            , ChoiceValue <$> genChoiceId <*> genNewValue
            , UseValue <$> genValueId
            ]
  | otherwise =
    oneOf $ pure SlotIntervalStart
      :| [ pure SlotIntervalEnd
        , AvailableMoney <$> genAccountId
        , Constant <$> genBigInteger
        , UseValue <$> genValueId
        ]

genObservation ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  m Observation
genObservation = genObservation' 5

genObservation' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  Int ->
  m Observation
genObservation' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genValue' newSize

        genNewObservation = genObservation' newSize
      in
        oneOf
          $ (AndObs <$> genNewObservation <*> genNewObservation)
          :| [ OrObs <$> genNewObservation <*> genNewObservation
            , NotObs <$> genNewObservation
            , ChoseSomething <$> genChoiceId
            , ValueGE <$> genNewValue <*> genNewValue
            , ValueGT <$> genNewValue <*> genNewValue
            , ValueLT <$> genNewValue <*> genNewValue
            , ValueLE <$> genNewValue <*> genNewValue
            , ValueEQ <$> genNewValue <*> genNewValue
            ]
  | otherwise = genLeaf
    where
    genLeaf ::
      m Observation
    genLeaf = ChoseSomething <$> genChoiceId

genContract ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Contract) =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  m Contract
genContract = genContract' 3

genContract' ::
  forall m.
  MonadGen m =>
  MonadRec m =>
  Lazy (m Contract) =>
  Lazy (m Observation) =>
  Lazy (m Value) =>
  Int ->
  m Contract
genContract' size
  | size > 1 =
    defer \_ ->
      let
        newSize = (size - 1)

        genNewValue = genValue' newSize

        genNewObservation = genObservation' newSize

        genNewContract = genContract' newSize
      in
        oneOf $ pure Refund
          :| [ Pay <$> genAccountId <*> genPayee <*> genNewValue <*> genNewContract
            , If <$> genNewObservation <*> genNewContract <*> genNewContract
            , When <$> genCases newSize <*> genTimeout <*> genNewContract
            , Let <$> genValueId <*> genNewValue <*> genNewContract
            ]
  | otherwise = genLeaf
    where
    genLeaf ::
      m Contract
    genLeaf = pure Refund
