-- | Vesting scheme as a Plutus contract
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Language.PlutusTx.Coordination.Contracts.Vesting (
    Vesting(..),
    VestingSchema,
    VestingTranche(..),
    VestingData(..),
    contract,
    totalAmount,
    validatorScriptHash,
    -- * Script
    validatorScript,
    mkValidator
    ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.State

import           GHC.Generics                 (Generic)
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx  as Tx
import           Language.PlutusTx.Prelude
import qualified Language.PlutusTx            as PlutusTx
import qualified Ledger                       as Ledger
import           Ledger                       (Address, DataScript (..), Slot(..), PubKey (..), RedeemerScript (..), ValidatorScript (..), TxOut)
import qualified Ledger.Ada                   as Ada
import           Ledger.AddressMap            (AddressMap)
import qualified Ledger.AddressMap            as AM
import qualified Ledger.Interval              as Interval
import qualified Ledger.Slot                  as Slot
import           Ledger.Scripts               (ValidatorHash, HashedDataScript)
import qualified Ledger.Scripts               as Scripts
import qualified Ledger.Tx                    as LTx
import           Ledger.Value                 (Value)
import qualified Ledger.Value                 as Value
import qualified Ledger.Validation            as Validation
import           Ledger.Validation            (PendingTx (..), PendingTxIn(..), PendingTxOut(..), getContinuingOutputs)
import qualified Prelude

-- | Tranche of a vesting scheme.
data VestingTranche = VestingTranche {
    vestingTrancheDate   :: Slot,
    vestingTrancheAmount :: Value
    } deriving Generic

PlutusTx.makeLift ''VestingTranche

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (slot) after which an additional amount can be spent.
data Vesting = Vesting {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner    :: PubKey
    } deriving Generic

PlutusTx.makeLift ''Vesting

{-# INLINABLE totalAmount #-}
-- | The total amount vested
totalAmount :: Vesting -> Value
totalAmount Vesting{vestingTranche1,vestingTranche2} =
    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

{-# INLINABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given slot range.
availableFrom :: VestingTranche -> Slot.SlotRange -> Value
availableFrom (VestingTranche d v) range =
    -- The valid range is an open-ended range starting from the tranche vesting date
    let validRange = Interval.from d
    -- If the valid range completely contains the argument range (meaning in particular
    -- that the start slot of the argument range is after the tranche vesting date), then
    -- the money in the tranche is available, otherwise nothing is available.
    in if validRange `Interval.contains` range then v else zero

-- | Data script for vesting utxo
data VestingData = VestingData {
    vestingDataHash    :: ValidatorHash, -- ^ Hash of the validator script
    vestingDataPaidOut :: Value          -- ^ How much of the vested value has already been retrieved
    } deriving (Generic)

instance Eq VestingData where
    {-# INLINABLE (==) #-}
    (VestingData h1 v1) == (VestingData h2 v2) = h1 == h2 && v1 == v2

PlutusTx.makeLift ''VestingData

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "vest funds" ()
        .\/ Endpoint "retrieve funds" Value

-- | The current state of the vesting contract
data VestingState =
    VestingState
        { knownOutputs :: AddressMap
        , vestingData  :: VestingData
        }

initialState :: Vesting -> VestingState
initialState vd =
    let hash = validatorScriptHash vd
        addr = Ledger.scriptAddress $ validatorScript vd
    in  VestingState
            { knownOutputs = AM.addAddress addr Prelude.mempty
            , vestingData  = VestingData hash mempty
            }

type MonadVesting m =
    ( MonadState VestingState m 
    , MonadReader Vesting m
    , MonadBase (Contract VestingSchema) m
    )

contractAddress :: MonadReader Vesting m => m Address
contractAddress = asks (Ledger.scriptAddress . validatorScript)
    
-- | Lock some Ada in the contract using the 'VestingData' as data script.
payIntoContract
    :: MonadReader Vesting m
    => VestingData
    -> Value
    -> m TxOut
payIntoContract vd vl = do
    address <- contractAddress
    let datScript = DataScript $ Ledger.lifted vd
    pure $ LTx.scriptTxOut' vl address datScript


vestFundsC
    :: ( MonadVesting m )
    => m ()
vestFundsC = do
    () <- liftBase (endpoint @"vest funds")
    h <- asks validatorScriptHash
    total <- asks totalAmount
    out <- payIntoContract (VestingData h mempty) total
    liftBase $ writeTx $ Tx.unbalancedTx [] [out]


retrieveFundsC
    :: ( MonadVesting m )
    => m ()
retrieveFundsC = do
    vl <- liftBase (endpoint  @"retrieve funds")
    currentSlot <- liftBase (awaitSlot 0)
    total <- asks totalAmount
    valScript <- asks validatorScript
    VestingState utxo vst <- get
    let ins = AM.spendScriptOutputs valScript redeemerScript utxo
        newState = vst { vestingDataPaidOut = vl + vestingDataPaidOut vst }
    outs <- payIntoContract newState (total - vestingDataPaidOut newState)
    liftBase $ writeTx $ Tx.unbalancedTx ins [outs] & Tx.validityRange .~ Interval.from currentSlot


updateStateC
    :: ( MonadVesting m )
    => m ()
updateStateC = do
    fundsVested <- asks totalAmount
    addr <- contractAddress
    tx <- liftBase (nextTransactionAt addr)
    VestingState oldUTXO oldState <- get
    let newUTXO = AM.updateAddresses tx oldUTXO
        remaining = AM.values newUTXO ^. at addr . _Just
        paidOutTotal = fundsVested - remaining
    put $ VestingState
            { knownOutputs = newUTXO
            , vestingData = VestingData (vestingDataHash oldState) paidOutTotal
            }

contract
    :: Vesting
    -> Contract VestingSchema ()
contract vst = runReaderT (evalStateT con (initialState vst)) vst where
    con = vestFundsC <|> forever (retrieveFundsC <|> updateStateC)
        
transactionFee :: Value
transactionFee = Ada.lovelaceValueOf 0

redeemerScript :: RedeemerScript
redeemerScript = RedeemerScript $ $$(Ledger.compileScript [|| \(_ :: Sealed (HashedDataScript VestingData)) -> () ||])

validatorScriptHash :: Vesting -> ValidatorHash
validatorScriptHash =
    Scripts.plcValidatorDigest
    . Ledger.getAddress
    . Ledger.scriptAddress
    . validatorScript

{-# INLINABLE mkValidator #-}
mkValidator :: Vesting -> VestingData -> () -> PendingTx -> Bool
mkValidator d@Vesting{vestingTranche1, vestingTranche2} VestingData{vestingDataPaidOut} () ptx@PendingTx{pendingTxValidRange = range} =
    let
        -- The locked funds which are returned?
        payBack :: Value
        payBack = mconcat (map pendingTxOutValue (getContinuingOutputs ptx))

        -- The funds available in the contract.
        lockedValue :: Value
        lockedValue = pendingTxInValue (pendingTxIn ptx)

        -- The funds that are paid to the owner
        amountSpent :: Value
        amountSpent = lockedValue - payBack

        -- Value that has been released so far under the scheme
        released = availableFrom vestingTranche1 range
            + availableFrom vestingTranche2 range

        paidOut = vestingDataPaidOut
        newAmount = paidOut + amountSpent

        -- Verify that the amount taken out, plus the amount already taken
        -- out before, does not exceed the threshold that is currently
        -- allowed
        amountsValid = newAmount `Value.leq` released

        -- Check that the remaining output is locked by the same validation
        -- script
        txnOutputsValid =
            let remaining = Validation.valueLockedBy ptx (Validation.ownHash ptx) in
            remaining == (totalAmount d - newAmount)

    in amountsValid && txnOutputsValid

validatorScript :: Vesting -> ValidatorScript
validatorScript v = ValidatorScript $
    $$(Ledger.compileScript [|| mkValidator ||])
        `Ledger.applyScript`
            Ledger.lifted v
