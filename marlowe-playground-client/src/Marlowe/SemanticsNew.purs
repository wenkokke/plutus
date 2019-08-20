-- https://github.com/input-output-hk/marlowe/blob/d4bd754e592004718c2b8bc34e2b30382a38193c/semantics-3.0/Semantics.hs
module Marlowe.SemanticsNew where

import Prelude
import Data.BigInteger (BigInteger)
import Data.Foldable (class Foldable, all, any, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Integral (class Integral)
import Data.Lens (Lens', over, set, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), mapMaybe, reverse, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Num (class Num)
import Data.Real (class Real)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))

type PubKey
  = BigInteger

type Party
  = PubKey

type Timeout
  = Slot

type Money
  = Ada

type ChosenNum
  = BigInteger

newtype Slot
  = Slot BigInteger

derive instance genericSlot :: Generic Slot _

derive instance newtypeSlot :: Newtype Slot _

derive instance eqSlot :: Eq Slot

derive instance ordSlot :: Ord Slot

instance showSlot :: Show Slot where
  show = genericShow

newtype Ada
  = Lovelace BigInteger

derive instance genericAda :: Generic Ada _

derive instance newtypeAda :: Newtype Ada _

derive instance eqAda :: Eq Ada

derive instance ordAda :: Ord Ada

instance showAda :: Show Ada where
  show = genericShow

derive newtype instance integralAda :: Integral Ada

derive newtype instance numAda :: Num Ada

derive newtype instance semiringAda :: Semiring Ada

derive newtype instance ringAda :: Ring Ada

derive newtype instance euclideanRingBlock :: EuclideanRing Ada

derive newtype instance realRingBlock :: Real Ada

instance commutativeRingAda :: CommutativeRing Ada

newtype AccountId
  = AccountId
  { accountNumber :: BigInteger
  , accountOwner :: PubKey
  }

derive instance genericAccountId :: Generic AccountId _

derive instance newtypeAccountId :: Newtype AccountId _

derive instance eqAccountId :: Eq AccountId

derive instance ordAccountId :: Ord AccountId

instance showAccountId :: Show AccountId where
  show (AccountId { accountNumber, accountOwner }) = "(AccountId " <> show accountNumber <> " " <> show accountOwner <> ")"

newtype ChoiceId
  = ChoiceId
  { choiceNumber :: BigInteger
  , choiceOwner :: PubKey
  }

derive instance genericChoiceId :: Generic ChoiceId _

derive instance newtypeChoiceId :: Newtype ChoiceId _

derive instance eqChoiceId :: Eq ChoiceId

derive instance ordChoiceId :: Ord ChoiceId

instance showChoiceId :: Show ChoiceId where
  show (ChoiceId { choiceNumber, choiceOwner }) = "(ChoiceId " <> show choiceNumber <> " " <> show choiceOwner <> ")"

newtype OracleId
  = OracleId PubKey

derive instance genericOracleId :: Generic OracleId _

derive instance newtypeOracleId :: Newtype OracleId _

derive instance eqOracleId :: Eq OracleId

derive instance ordOracleId :: Ord OracleId

instance showOracleId :: Show OracleId where
  show = genericShow

newtype ValueId
  = ValueId BigInteger

derive instance genericValueId :: Generic ValueId _

derive instance newtypeValueId :: Newtype ValueId _

derive instance eqValueId :: Eq ValueId

derive instance ordValueId :: Ord ValueId

instance showValueId :: Show ValueId where
  show = genericShow

data Value
  = AvailableMoney AccountId
  | Constant BigInteger
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | ChoiceValue ChoiceId Value
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue ValueId

derive instance genericValue :: Generic Value _

derive instance eqValue :: Eq Value

derive instance ordValue :: Ord Value

instance showValue :: Show Value where
  show v = genericShow v

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs

derive instance genericObservation :: Generic Observation _

derive instance eqObservation :: Eq Observation

derive instance ordObservation :: Ord Observation

instance showObservation :: Show Observation where
  show o = genericShow o

-- |Interval of [from, to], both bounds are included
newtype Interval a
  = Interval { from :: a, to :: a }

derive instance genericInterval :: Generic (Interval a) _

derive instance newtypeInterval :: Newtype (Interval a) _

derive instance eqInterval :: Eq a => Eq (Interval a)

derive instance ordInterval :: Ord a => Ord (Interval a)

instance showInterval :: (Show a, Generic a rep) => Show (Interval a) where
  show v = genericShow v

_from :: forall a. Lens' (Interval a) a
_from = _Newtype <<< prop (SProxy :: SProxy "from")

_to :: forall a. Lens' (Interval a) a
_to = _Newtype <<< prop (SProxy :: SProxy "to")

validInterval :: forall a. Ord a => Interval a -> Boolean
validInterval (Interval interval) = interval.from <= interval.to

above :: forall a. Ord a => Interval a -> a -> Boolean
above (Interval interval) v = v >= interval.to

anyWithin :: forall f a. Foldable f => Ord a => a -> f (Interval a) -> Boolean
anyWithin v = any (\(Interval interval) -> v >= interval.from && v <= interval.to)

type SlotInterval
  = Interval Slot

type Bound
  = Interval BigInteger

data Action
  = Deposit AccountId Party Value
  | Choice ChoiceId (List Bound)
  | Notify Observation

derive instance genericAction :: Generic Action _

derive instance eqAction :: Eq Action

derive instance ordAction :: Ord Action

instance showAction :: Show Action where
  show v = genericShow v

data Payee
  = Account AccountId
  | Party Party

derive instance genericPayee :: Generic Payee _

derive instance eqPayee :: Eq Payee

derive instance ordPayee :: Ord Payee

instance showPayee :: Show Payee where
  show v = genericShow v

data Contract
  = Refund
  | Pay AccountId Payee Value Contract
  | If Observation Contract Contract
  | When (List Case) Timeout Contract
  | Let ValueId Value Contract

derive instance genericContract :: Generic Contract _

derive instance eqContract :: Eq Contract

derive instance ordContract :: Ord Contract

instance showContract :: Show Contract where
  show v = genericShow v

newtype Case
  = Case
  { action :: Action
  , contract :: Contract
  }

derive instance genericCase :: Generic Case _

derive instance newtypeCase :: Newtype Case _

derive instance eqCase :: Eq Case

derive instance ordCase :: Ord Case

instance showCase :: Show Case where
  show (Case { action, contract }) = "(Case " <> show action <> " " <> show contract <> ")"

newtype State
  = State
  { accounts :: Map AccountId Money
  , choices :: Map ChoiceId ChosenNum
  , boundValues :: Map ValueId BigInteger
  , minSlot :: Slot
  }

derive instance genericState :: Generic State _

derive instance newtypeState :: Newtype State _

derive instance eqState :: Eq State

derive instance ordState :: Ord State

instance showState :: Show State where
  show v = genericShow v

_accounts :: Lens' State (Map AccountId Money)
_accounts = _Newtype <<< prop (SProxy :: SProxy "accounts")

_choices :: Lens' State (Map ChoiceId ChosenNum)
_choices = _Newtype <<< prop (SProxy :: SProxy "choices")

_boundValues :: Lens' State (Map ValueId BigInteger)
_boundValues = _Newtype <<< prop (SProxy :: SProxy "boundValues")

newtype Environment
  = Environment { slotInterval :: SlotInterval }

derive instance genericEnvironment :: Generic Environment _

derive instance newtypeEnvironment :: Newtype Environment _

derive instance eqEnvironment :: Eq Environment

derive instance ordEnvironment :: Ord Environment

instance showEnvironment :: Show Environment where
  show v = genericShow v

_slotInterval :: Lens' Environment SlotInterval
_slotInterval = _Newtype <<< prop (SProxy :: SProxy "slotInterval")

data Input
  = IDeposit AccountId Party Money
  | IChoice ChoiceId ChosenNum
  | INotify

derive instance genericInput :: Generic Input _

derive instance eqInput :: Eq Input

derive instance ordInput :: Ord Input

instance showInput :: Show Input where
  show v = genericShow v

-- Processing of slot interval
data IntervalError
  = InvalidInterval SlotInterval
  | IntervalInPastError Slot SlotInterval

derive instance genericIntervalError :: Generic IntervalError _

derive instance eqIntervalError :: Eq IntervalError

derive instance ordIntervalError :: Ord IntervalError

instance showIntervalError :: Show IntervalError where
  show v = genericShow v

data IntervalResult
  = IntervalTrimmed Environment State
  | IntervalError IntervalError

derive instance genericIntervalResult :: Generic IntervalResult _

derive instance eqIntervalResult :: Eq IntervalResult

derive instance ordIntervalResult :: Ord IntervalResult

instance showIntervalResult :: Show IntervalResult where
  show v = genericShow v

-- Note: We use guards here because currently nested ifs break purty formatting
--       We need to upgrade purty and purescript to fix
fixInterval :: SlotInterval -> State -> IntervalResult
fixInterval interval (State state)
  | (not <<< validInterval) interval = IntervalError (InvalidInterval interval)
  | above interval state.minSlot = IntervalError (IntervalInPastError state.minSlot interval)
  | otherwise =
    let
      -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
      newLow = max (unwrap interval).from state.minSlot

      -- We know high is greater or equal than newLow (prove)
      currentInterval = Interval { from: newLow, to: (unwrap interval).to }

      env = Environment { slotInterval: currentInterval }

      newState = State (state { minSlot = newLow })
    in
      IntervalTrimmed env newState

-- EVALUATION
-- | Evaluate a @Value@ to Integer
evalValue :: Environment -> State -> Value -> BigInteger
evalValue env state value =
  let
    eval = evalValue env state
  in
    case value of
      AvailableMoney accId ->
        let
          balance = fromMaybe zero $ Map.lookup accId (unwrap state).accounts
        in
          unwrap balance
      Constant integer -> integer
      NegValue val -> eval val
      AddValue lhs rhs -> eval lhs + eval rhs
      SubValue lhs rhs -> eval lhs + eval rhs
      ChoiceValue choiceId defVal -> fromMaybe (eval defVal) $ Map.lookup choiceId (unwrap state).choices
      SlotIntervalStart -> view (_slotInterval <<< _from <<< _Newtype) env
      SlotIntervalEnd -> view (_slotInterval <<< _to <<< _Newtype) env
      UseValue valId -> fromMaybe zero $ Map.lookup valId (unwrap state).boundValues

-- | Evaluate an @Observation@ to Bool
evalObservation :: Environment -> State -> Observation -> Boolean
evalObservation env state obs =
  let
    evalObs = evalObservation env state

    evalVal = evalValue env state
  in
    case obs of
      AndObs lhs rhs -> evalObs lhs && evalObs rhs
      OrObs lhs rhs -> evalObs lhs || evalObs rhs
      NotObs subObs -> not (evalObs subObs)
      ChoseSomething choiceId -> choiceId `Map.member` (unwrap state).choices
      ValueGE lhs rhs -> evalVal lhs >= evalVal rhs
      ValueGT lhs rhs -> evalVal lhs > evalVal rhs
      ValueLT lhs rhs -> evalVal lhs < evalVal rhs
      ValueLE lhs rhs -> evalVal lhs <= evalVal rhs
      ValueEQ lhs rhs -> evalVal lhs == evalVal rhs
      TrueObs -> true
      FalseObs -> false

-- | Pick the first account with money in it
refundOne :: Map AccountId Money -> Maybe (Tuple (Tuple Party Money) (Map AccountId Money))
refundOne accounts = do
  { key, value } <- Map.findMin accounts
  let
    rest = Map.delete key accounts
  if value > zero then pure (Tuple (Tuple (unwrap key).accountOwner value) rest) else refundOne rest

-- | Obtains the amount of money available an account
moneyInAccount :: AccountId -> Map AccountId Money -> Money
moneyInAccount accId accounts = fromMaybe zero (Map.lookup accId accounts)

-- | Sets the amount of money available in an account
updateMoneyInAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
updateMoneyInAccount accId money = if money <= zero then Map.delete accId else Map.insert accId money

{-| Withdraw up to the given amount of money from an account
    Return the amount of money withdrawn
-}
withdrawMoneyFromAccount ::
  AccountId -> Money -> Map AccountId Money -> Tuple Money (Map AccountId Money)
withdrawMoneyFromAccount accId money accounts =
  let
    balance = moneyInAccount accId accounts

    withdrawnMoney = min balance money

    newBalance = balance - withdrawnMoney

    newAcc = updateMoneyInAccount accId newBalance accounts
  in
    Tuple withdrawnMoney newAcc

{-| Add the given amount of money to an account (only if it is positive).
    Return the updated Map
-}
addMoneyToAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
addMoneyToAccount accId money accounts =
  let
    balance = moneyInAccount accId accounts

    newBalance = balance + money
  in
    if money <= zero then
      accounts
    else
      updateMoneyInAccount accId newBalance accounts

{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
giveMoney :: Payee -> Money -> Map AccountId Money -> Tuple ReduceEffect (Map AccountId Money)
giveMoney payee money accounts = case payee of
  Party party -> Tuple (Just (Payment party money)) accounts
  Account accId ->
    let
      newAccs = addMoneyToAccount accId money accounts
    in
      Tuple Nothing newAccs

data Payment
  = Payment Party Money

derive instance genericPayment :: Generic Payment _

derive instance eqPayment :: Eq Payment

derive instance ordPayment :: Ord Payment

instance showPayment :: Show Payment where
  show = genericShow

type ReduceEffect
  = Maybe Payment

data ReduceWarning
  = ReduceNoWarning
  | ReduceNonPositivePay AccountId Payee Money
  | ReducePartialPay AccountId Payee Money Money
  -- ^ src    ^ dest ^ paid ^ expected
  | ReduceShadowing ValueId BigInteger BigInteger

-- oldVal ^  newVal ^
derive instance genericReduceWarning :: Generic ReduceWarning _

derive instance eqReduceWarning :: Eq ReduceWarning

derive instance ordReduceWarning :: Ord ReduceWarning

instance showReduceWarning :: Show ReduceWarning where
  show = genericShow

data ReduceResult
  = Reduced ReduceWarning ReduceEffect State Contract
  | NotReduced
  | AmbiguousSlotIntervalError

derive instance genericReduceResult :: Generic ReduceResult _

derive instance eqReduceResult :: Eq ReduceResult

derive instance ordReduceResult :: Ord ReduceResult

instance showReduceResult :: Show ReduceResult where
  show = genericShow

-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceResult
reduceContractStep env state contract = case contract of
  Refund -> case refundOne (unwrap state).accounts of
    Just (Tuple (Tuple party money) newAccounts) ->
      let
        oldState = unwrap state

        newState = wrap (oldState { accounts = newAccounts })
      in
        Reduced ReduceNoWarning (Just (Payment party money)) newState Refund
    Nothing -> NotReduced
  Pay accId payee val nextContract ->
    let
      moneyToPay = Lovelace (evalValue env state val)
    in
      if moneyToPay <= zero then
        Reduced (ReduceNonPositivePay accId payee moneyToPay) Nothing state nextContract
      else
        let
          (Tuple paidMoney newAccounts) = withdrawMoneyFromAccount accId moneyToPay (unwrap state).accounts

          warning = if paidMoney < moneyToPay then ReducePartialPay accId payee paidMoney moneyToPay else ReduceNoWarning

          (Tuple payEffect finalAccounts) = giveMoney payee paidMoney newAccounts

          newState = set _accounts finalAccounts state
        in
          Reduced warning payEffect newState nextContract
  If observation contract1 contract2 ->
    let
      nextContract = if evalObservation env state observation then contract1 else contract2
    in
      Reduced ReduceNoWarning Nothing state nextContract
  -- TODO: Why isn't cases used?
  When _ timeout nextContract ->
    let
      startSlot = view (_slotInterval <<< _from) env

      endSlot = view (_slotInterval <<< _to) env
    in
      if endSlot < timeout then
        NotReduced
      else if timeout <= startSlot then
        Reduced ReduceNoWarning Nothing state nextContract
      else
        AmbiguousSlotIntervalError
  Let valId val nextContract ->
    let
      evaluatedValue = evalValue env state val

      newState = over _boundValues (Map.insert valId evaluatedValue) state

      warn = case Map.lookup valId (unwrap state).boundValues of
        Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
        Nothing -> ReduceNoWarning
    in
      Reduced warn Nothing newState nextContract

data ReduceAllResult
  = ReduceAllSuccess (List ReduceWarning) (List Payment) State Contract
  | ReduceAllAmbiguousSlotIntervalError

derive instance genericReduceAllResult :: Generic ReduceAllResult _

derive instance eqReduceAllResult :: Eq ReduceAllResult

derive instance ordReduceAllResult :: Ord ReduceAllResult

instance showReduceAllResult :: Show ReduceAllResult where
  show = genericShow

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceAllResult
reduceContractUntilQuiescent startEnv startState startContract =
  let
    reduceAll ::
      Environment -> State -> Contract -> (List ReduceWarning) -> (List Payment) -> ReduceAllResult
    reduceAll env state contract warnings effects = case reduceContractStep env state contract of
      Reduced warning effect newState nextContract ->
        let
          newWarnings = if warning == ReduceNoWarning then warnings else warning : warnings

          newEffects = case effect of
            Just eff -> eff : effects
            Nothing -> effects
        in
          reduceAll env newState nextContract newWarnings newEffects
      AmbiguousSlotIntervalError -> ReduceAllAmbiguousSlotIntervalError
      -- this is the last invocation of reduceAll, so we can reverse lists
      NotReduced -> ReduceAllSuccess (reverse warnings) (reverse effects) state contract
  in
    reduceAll startEnv startState startContract mempty mempty

data ApplyResult
  = Applied State Contract
  | ApplyNoMatchError

derive instance genericApplyResult :: Generic ApplyResult _

derive instance eqApplyResult :: Eq ApplyResult

derive instance ordApplyResult :: Ord ApplyResult

instance showApplyResult :: Show ApplyResult where
  show = genericShow

applyCases :: Environment -> State -> Input -> List Case -> ApplyResult
applyCases env state input cases = case input, cases of
  IDeposit accId1 party1 money, (Case { action: Deposit accId2 party2 val, contract: cont } : rest) ->
    let
      amount = evalValue env state val

      newState = over _accounts (addMoneyToAccount accId1 money) state
    in
      if accId1 == accId2 && party1 == party2 && unwrap money == amount then
        Applied newState cont
      else
        applyCases env state input rest
  IChoice choId1 choice, (Case { action: Choice choId2 bounds, contract: cont } : rest) ->
    let
      newState = over _choices (Map.insert choId1 choice) state
    in
      if choId1 == choId2 && anyWithin choice bounds then
        Applied newState cont
      else
        applyCases env state input rest
  _, (Case { action: Notify obs, contract: cont } : _)
    | evalObservation env state obs -> Applied state cont
  _, (_ : rest) -> applyCases env state input rest
  _, Nil -> ApplyNoMatchError

applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input cases

applyInput _ _ _ _ = ApplyNoMatchError

data ApplyAllResult
  = ApplyAllSuccess (List ReduceWarning) (List Payment) State Contract
  | ApplyAllNoMatchError
  | ApplyAllAmbiguousSlotIntervalError

derive instance genericApplyAllResult :: Generic ApplyAllResult _

derive instance eqApplyAllResult :: Eq ApplyAllResult

derive instance ordApplyAllResult :: Ord ApplyAllResult

instance showApplyAllResult :: Show ApplyAllResult where
  show = genericShow

-- | Apply a list of Inputs to the contract
applyAllInputs :: Environment -> State -> Contract -> (List Input) -> ApplyAllResult
applyAllInputs startEnv startState startContract startInputs =
  let
    applyAllAux ::
      Environment ->
      State ->
      Contract ->
      List Input ->
      List ReduceWarning ->
      List Payment ->
      ApplyAllResult
    applyAllAux env state contract inputs warnings effects = case reduceContractUntilQuiescent env state contract of
      ReduceAllAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
      ReduceAllSuccess warns effs curState cont -> case inputs of
        Nil -> ApplyAllSuccess (warnings <> warns) (effects <> effs) curState cont
        (input : rest) -> case applyInput env curState input cont of
          Applied newState nextContract -> applyAllAux env newState nextContract rest (warnings <> warns) (effects <> effs)
          ApplyNoMatchError -> ApplyAllNoMatchError
  in
    applyAllAux startEnv startState startContract startInputs mempty mempty

-- PROCESS
data ProcessError
  = PEAmbiguousSlotIntervalError
  | PEApplyNoMatchError
  | PEIntervalError IntervalError
  | PEUselessTransaction

derive instance genericProcessError :: Generic ProcessError _

derive instance eqProcessError :: Eq ProcessError

derive instance ordProcessError :: Ord ProcessError

instance showProcessError :: Show ProcessError where
  show = genericShow

data ProcessResult
  = Processed
    (List ReduceWarning)
    (List Payment)
    TransactionOutcomes
    State
    Contract
  | ProcessError ProcessError

derive instance genericProcessResult :: Generic ProcessResult _

derive instance eqProcessResult :: Eq ProcessResult

derive instance ordProcessResult :: Ord ProcessResult

instance showProcessResult :: Show ProcessResult where
  show = genericShow

newtype Transaction
  = Transaction
  { interval :: SlotInterval
  , inputs :: (List Input)
  }

derive instance genericTransaction :: Generic Transaction _

derive instance newtypeTransaction :: Newtype Transaction _

derive instance eqTransaction :: Eq Transaction

derive instance ordTransaction :: Ord Transaction

instance showTransaction :: Show Transaction where
  show = genericShow

type TransactionOutcomes
  = Map Party Money

isEmptyOutcome :: TransactionOutcomes -> Boolean
isEmptyOutcome trOut = all ((==) zero) trOut

-- Adds a value to the map of outcomes
addOutcome :: Party -> Money -> TransactionOutcomes -> TransactionOutcomes
addOutcome party diffValue trOut =
  let
    f (Just value) = Just (value + diffValue)

    f Nothing = Just diffValue
  in
    Map.alter f party trOut

-- | Extract total outcomes from transaction inputs and outputs
getOutcomes :: List Payment -> List Input -> TransactionOutcomes
getOutcomes effect input =
  let
    -- turn both effects and inputs into the same type: Tuple party money
    outcomes = map (\(Payment party money) -> Tuple party money) effect

    isDeposit (IDeposit _ party money) = Just (Tuple party money)

    isDeposit _ = Nothing

    incomes = mapMaybe isDeposit input
  in
    foldl (\acc (Tuple party money) -> addOutcome party money acc)
      mempty
      (outcomes <> incomes)

-- | Try to process a transaction
processTransaction :: Transaction -> State -> Contract -> ProcessResult
processTransaction tx state contract =
  let
    inputs = (unwrap tx).inputs
  in
    case fixInterval (unwrap tx).interval state of
      IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
        ApplyAllSuccess warnings effects newState cont -> case contract == cont of -- purty breaks with `if` until we upgrade to PS 0.13
          true -> ProcessError PEUselessTransaction
          false ->
            let
              outcomes = getOutcomes effects inputs
            in
              Processed warnings effects outcomes newState cont
        ApplyAllNoMatchError -> ProcessError PEApplyNoMatchError
        ApplyAllAmbiguousSlotIntervalError -> ProcessError PEAmbiguousSlotIntervalError
      IntervalError error -> ProcessError (PEIntervalError error)
