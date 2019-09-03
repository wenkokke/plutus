{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
module Spec.Vesting(tests) where

import           Test.Tasty
import qualified Test.Tasty.HUnit                                 as HUnit

import           Spec.Lib                                         as Lib

import qualified Language.PlutusTx                                as PlutusTx
import           Language.PlutusTx.Lattice
import qualified Language.PlutusTx.Numeric                        as Numeric
import           Language.PlutusTx.Coordination.Contracts.Vesting (Vesting (..), VestingTranche (..), VestingSchema, contract,
                                                                   mkValidator, totalAmount, validatorScript)
import qualified Ledger
import qualified Ledger.Ada                                       as Ada
import           Wallet.Emulator

import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Trace                   as Trace

wallet1, wallet2 :: Wallet
wallet1 = Wallet 1
wallet2 = Wallet 2

tests :: TestTree
tests =
    let con = contract vesting in
    testGroup "vesting"
    [ checkPredicate "secure some funds with the vesting script"
        con
        (walletFundsChange wallet2 (Numeric.negate $ totalAmount vesting))
        initialiseTrace

    , checkPredicate "retrieve some funds"
        con
        (walletFundsChange wallet2 (Numeric.negate $ totalAmount vesting)
            /\ walletFundsChange wallet1 (Ada.lovelaceValueOf 100))
        $ initialiseTrace
            >> Trace.addBlocks 10
            >> Trace.callEndpoint @"retrieve funds" wallet1 (Ada.lovelaceValueOf 100)
            >> Trace.notifySlot wallet1
            >> Trace.handleBlockchainEvents wallet1
            >> Trace.addBlocks 2

    , checkPredicate "cannot retrieve more than allows"
        con
        (walletFundsChange wallet1 mempty)
        $ initialiseTrace
            >> Trace.addBlocks 10
            >> Trace.callEndpoint @"retrieve funds" wallet1 (Ada.lovelaceValueOf 300)
            >> Trace.notifySlot wallet1
            >> Trace.handleBlockchainEvents wallet1
            >> Trace.addBlocks 2

    , checkPredicate "can retrieve everything at the end"
        con
        (walletFundsChange wallet1 (totalAmount vesting))
        $ initialiseTrace
            >> Trace.addBlocks 20
            >> Trace.callEndpoint @"retrieve funds" wallet1 (totalAmount vesting)
            >> Trace.notifySlot wallet1
            >> Trace.handleBlockchainEvents wallet1
            >> Trace.addBlocks 2

    , Lib.goldenPir "test/Spec/vesting.pir" $$(PlutusTx.compile [|| mkValidator ||])
    , HUnit.testCase "script size is reasonable" (Lib.reasonable (validatorScript vesting) 45000)
    ]


initialiseTrace
    :: ( MonadEmulator m )
    => ContractTrace VestingSchema m a ()
initialiseTrace =
    Trace.notifyInterestingAddresses wallet1
    >> Trace.callEndpoint @"vest funds" wallet2 ()
    >> Trace.handleBlockchainEvents wallet2

-- | The scenario used in the property tests. It sets up a vesting scheme for a
--   total of 600 ada over 20 blocks (200 ada can be taken out before that, at
--   10 blocks).
vesting :: Vesting
vesting =
    Vesting
        { vestingTranche1 = VestingTranche (Ledger.Slot 10) (Ada.lovelaceValueOf 200)
        , vestingTranche2 = VestingTranche (Ledger.Slot 20) (Ada.lovelaceValueOf 400)
        , vestingOwner    = walletPubKey wallet1 }
