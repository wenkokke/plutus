{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Language.Marlowe3.Client where
import           Control.Applicative            ( Applicative(..) )
import           Control.Monad                  ( Monad(..)
                                                , void
                                                , when
                                                )
import           Control.Monad.Error.Class      ( MonadError(..) )
import           Data.Maybe                     ( maybeToList )
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           Wallet                         ( WalletAPI(..)
                                                , WalletAPIError
                                                , intervalFrom
                                                , throwOtherError
                                                , createTxAndSubmit
                                                , ownPubKeyTxOut
                                                )
import           Ledger                         ( DataScript(..)
                                                , PubKey(..)
                                                , Slot(..)
                                                , Tx
                                                , TxOutRef
                                                , TxIn
                                                , TxOut
                                                , TxOutOf(..)
                                                , ValidatorScript(..)
                                                , HashedDataScript(..)
                                                , interval
                                                , plcValidatorHash
                                                , pubKeyTxOut
                                                , scriptTxIn
                                                , scriptTxOut
                                                )
import qualified Ledger                         as Ledger
import           Ledger.Ada                     (Ada)
import qualified Ledger.Ada                     as Ada
import           Ledger.Tx
import qualified Language.PlutusTx.Prelude      as P
import           Language.Marlowe3.Common       as Marlowe
import Debug.Trace


createContract :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Contract
    -> m MarloweData
createContract contract = do
    slot <- slot
    creator <- ownPubKey
    let validator = validatorScript creator

        marloweData = MarloweData {
            marloweCreator = creator,
            marloweContract = contract,
            marloweState = emptyState }
        ds = DataScript $ Ledger.lifted marloweData

        deposit = Ada.adaValueOf 1

    (payment, change) <- createPaymentWithChange deposit
    let o = scriptTxOut deposit validator ds
        slotRange = interval slot (slot + 10)
        outputs = o : maybeToList change

    void $ createTxAndSubmit slotRange payment outputs
    return marloweData


deposit :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Tx
    -> MarloweData
    -> AccountId
    -> Ada
    -> m MarloweData
deposit tx marloweData accountId amount = do
    pubKey <- ownPubKey
    applyInputs tx marloweData [IDeposit accountId pubKey amount]


applyInputs :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Tx
    -> MarloweData
    -> [Input]
    -> m MarloweData
applyInputs tx MarloweData{..} inputs  = do
    let depositAmount = Ada.adaOf 1
    let depositPayment = Payment marloweCreator depositAmount
    let validator = validatorScript marloweCreator
    let address = scriptAddress validator
    slot <- slot

    let slotRange = interval slot (slot + Slot 10)
    let txInput = TransactionInput {
            txInterval = (slot, slot + Slot 10),
            txInputs = inputs }

    let [(_, ref)] = filter (isAddress address) (txOutRefs tx)

    let computedResult = computeTransaction txInput marloweState marloweContract

    (scriptIn, deducedTxOutputs, marloweData) <- case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweCreator = marloweCreator,
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (scriptIn, deducedTxOutputs) = case txOutContract of
                    Refund -> let
                        redcode = $$(Ledger.compileScript [||
                            \(input :: [Input]) -> (input, Nothing :: Maybe (P.Sealed (HashedDataScript MarloweData))) ||])
                                `Ledger.applyScript` (Ledger.lifted inputs)
                        redeemer = Ledger.RedeemerScript redcode
                        scriptIn = scriptTxIn ref validator redeemer

                        in (scriptIn, txPaymentOuts (depositPayment : txOutPayments))

                    _ -> let

                        redcode = $$(Ledger.compileScript [||
                            \(input :: [Input]) (sealedDS :: P.Sealed (HashedDataScript MarloweData)) -> (input, Just sealedDS) ||])
                                `Ledger.applyScript` (Ledger.lifted inputs)
                        redeemer = Ledger.RedeemerScript redcode
                        scriptIn = scriptTxIn ref validator redeemer

                        payoutAmounts = fmap (Ada.getLovelace . Ada.fromValue . txOutValue) (txPaymentOuts txOutPayments)
                        totalPayouts = sum payoutAmounts
                        finalBalance = totalIncome - totalPayouts + Ada.getLovelace depositAmount
                        dataScript = DataScript (Ledger.lifted marloweData)
                        scritOutValue = Ada.lovelaceValueOf finalBalance
                        scritOut = scriptTxOut scritOutValue validator dataScript
                        in (scriptIn, scritOut : txPaymentOuts txOutPayments)

            return (scriptIn, deducedTxOutputs, marloweData)
        Error txError -> throwOtherError (Text.pack $ show txError)



    (payment, change) <- createPaymentWithChange (Ada.lovelaceValueOf totalIncome)

    void $ createTxAndSubmit
        slotRange
        (Set.insert scriptIn payment)
        (deducedTxOutputs ++ maybeToList change)

    return marloweData
  where
    collectDeposits (IDeposit _ _ money) acc = Ada.getLovelace money + acc
    collectDeposits _ acc = acc

    totalIncome = foldr collectDeposits 0 inputs

    isAddress address (TxOutOf{txOutAddress}, _) = txOutAddress == address

    txPaymentOuts :: [Payment] -> [TxOut]
    txPaymentOuts payments = let
        ps = foldr collectPayments Map.empty payments
        txOuts = [pubKeyTxOut (Ada.toValue value) pk | (pk, value) <- Map.toList ps]
        in txOuts

    collectPayments :: Payment -> Map Party Ada -> Map Party Ada
    collectPayments (Payment party money) payments = let
        newValue = case Map.lookup party payments of
            Just value -> value + money
            Nothing -> money
        in Map.insert party newValue payments


