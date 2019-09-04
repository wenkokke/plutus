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
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map
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
                                                , interval
                                                , pubKeyTxOut
                                                , scriptTxIn
                                                , scriptTxOut
                                                )
import qualified Ledger                         as Ledger
import           Ledger.Ada                     (Ada)
import qualified Ledger.Ada                     as Ada
import           Ledger.Validation
import           Language.Marlowe3.Common


getScriptOutFromTx :: Tx -> (TxOut, TxOutRef)
getScriptOutFromTx = head . filter (Ledger.isPayToScriptOut . fst) . Ledger.txOutRefs


createContract :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Contract
    -> m ()
createContract contract = do
    slot <- slot
    creator <- ownPubKey
    let validator = validatorScript creator
    let ds = DataScript $ Ledger.lifted MarloweData {
            marloweCreator = creator,
            marloweContract = contract,
            marloweState = emptyState }
    let v' = Ada.adaValueOf 1
    (payment, change) <- createPaymentWithChange v'
    let o = scriptTxOut v' validator ds

    void $ createTxAndSubmit (interval slot (slot + 10)) payment (o : maybeToList change)


marloweTx ::
    ([Input], MarloweData)
    -- ^ redeemer is passed here
    -> (TxOut, TxOutRef)
    -- ^ reference to Marlowe contract UTxO
    -> ValidatorScript
    -- ^ actuall contract script
    -> (TxIn -> (Integer -> TxOut) -> Integer -> m ())
    -- ^ do wallet actions given Marlowe contract 'TxIn', contract 'TxOut' generator,
    --   and current contract money
    -> m ()
marloweTx inputState txOut validator f = let
    (TxOutOf _ vl _, ref) = txOut
    contractValue = Ada.getLovelace $ Ada.fromValue vl
    lifted = Ledger.lifted inputState
    scriptIn = scriptTxIn ref validator $ Ledger.RedeemerScript lifted
    dataScript = DataScript lifted
    scritOut v = scriptTxOut (Ada.lovelaceValueOf v) validator dataScript
    in f scriptIn scritOut contractValue


-- | Create Marlowe Redeemer Script as @(Input, MarloweData)@.
createRedeemer
    :: PubKey -> [Input] -> State -> Contract -> ([Input], MarloweData)
createRedeemer creator inputs expectedState expectedCont =
    let mdata = MarloweData {
        marloweCreator = creator,
        marloweContract = expectedCont,
        marloweState = expectedState }
    in  (inputs, mdata)


createDeposit tx MarloweData{..} validator accountId value  = do
    when (value <= 0) $ throwOtherError "Must deposit a positive value"
    let (TxHash hash) = plcTxHash . Ledger.hashTx $ tx
    sig <- sign hash
    slot <- slot
    pubKey <- ownPubKey

    let slotInterval = interval slot (slot + Slot 10)
    let inputs = [IDeposit accountId pubKey (Ada.lovelaceOf value)]
    let txInput = TransactionInput { txInterval = (slot, slot + Slot 10), txInputs = inputs }

    let TransactionOutput {txOutPayments, txOutState, txOutContract} =
            computeTransaction txInput marloweState marloweContract

    let collectPayments payments (Payment party money) = let
            newValue = case Map.lookup party payments of
                Just value -> value + money
                Nothing -> money
            in Map.insert party newValue payments


    let payments = foldl collectPayments Map.empty txOutPayments
    let txOuts = fmap (\(pk, value) -> pubKeyTxOut (Ada.toValue value) pk) (Map.toList payments)

    let redeemer = createRedeemer
            pubKey
            inputs
            txOutState
            txOutContract
    let txOut = getScriptOutFromTx tx
    marloweTx redeemer txOut validator $ \ contractTxIn getTxOut contractValue -> do
        (payment, change) <- createPaymentWithChange (Ada.lovelaceValueOf value)
        void $ createTxAndSubmit
            slotInterval
            (Set.insert contractTxIn payment)
            (getTxOut (contractValue + value) : maybeToList change ++ txOuts)

