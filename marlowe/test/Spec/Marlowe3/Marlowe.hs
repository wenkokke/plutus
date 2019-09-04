{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns
-fno-warn-name-shadowing
-fno-warn-unused-do-bind
-fno-warn-unused-top-binds #-}
module Spec.Marlowe3.Marlowe
    ( tests
    )
where

import           Control.Monad              (void)
import           Data.Either                (isRight)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map

import           Hedgehog                   (Gen, Property, Size (..), forAll, property)
import qualified Hedgehog
import           Hedgehog.Gen               (choice, element, integral, sized)
import qualified Hedgehog.Range             as Range

import           Language.Marlowe3.Common
import           Language.Marlowe3.Client
import           Ledger                     hiding (Value)
import qualified Ledger
import qualified Ledger.Ada                 as Ada
import           Ledger.Crypto
import           Wallet                     (PubKey (..))
import           Wallet.Emulator
import qualified Wallet.Emulator.Generators as Gen
import qualified Wallet.Generators          as Gen
import           Test.Tasty
import           Test.Tasty.Hedgehog       (HedgehogTestLimit (..), testProperty)
import           Test.Tasty.HUnit
import Debug.Trace



newtype MarloweScenario = MarloweScenario { mlInitialBalances :: Map PubKey Ledger.Value }

tests :: TestTree
tests = testGroup "Marlowe"
    [ testCase "Contracts with different creators have different hashes" uniqueContractHash
    , testProperty "Escrow Contract" simpleTest
    ]


alice, bob, carol :: Wallet
alice = Wallet 1
bob = Wallet 2
carol = Wallet 3

simpleTest :: Property
simpleTest = checkMarloweTrace (MarloweScenario {
    mlInitialBalances = Map.fromList [ (walletPubKey alice, Ada.adaValueOf 1000)  ] }) $ do
    -- Init a contract
    let alicePk = walletPubKey alice
        aliceAcc = AccountId 0 alicePk
        update = updateAll [alice]
    update

    let contract = When [
                (Deposit aliceAcc alicePk (Constant 500_000_000), Refund)
            ] (Slot 100) Refund
    (tx, md) <- alice `performs` createContract contract
    alice `performs` deposit tx md aliceAcc (Ada.adaOf 500)

    assertOwnFundsEq alice (Ada.adaValueOf 1000)


pubKeyGen :: Gen PubKey
pubKeyGen = toPublicKey . (knownPrivateKeys !!) <$> integral (Range.linear 0 10)

uniqueContractHash :: IO ()
uniqueContractHash = do
    let pk1 = toPublicKey privateKey1
    let pk2 = toPublicKey privateKey2
    let hash1 = plcValidatorHash $ validatorScript pk1
    let hash2 = plcValidatorHash $ validatorScript pk2
    hash1 == hash2 @?= False


-- | Run a trace with the given scenario and check that the emulator finished
--   successfully with an empty transaction pool.
checkMarloweTrace :: MarloweScenario -> Trace MockWallet () -> Property
checkMarloweTrace MarloweScenario{mlInitialBalances} t = property $ do
    let model = Gen.generatorModel { Gen.gmInitialBalance = mlInitialBalances }
    (result, st) <- forAll $ Gen.runTraceOn model t
    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == _txPool st)


updateAll :: [Wallet] -> Trace MockWallet ()
updateAll wallets = processPending >>= void . walletsNotifyBlock wallets

performs :: Wallet -> m MarloweData -> Trace m (Tx, MarloweData)
performs actor action = do
    (md, txs) <- walletAction actor action
    let tx = head txs
    processPending >>= void . walletsNotifyBlock [actor]
    assertIsValidated tx
    return (tx, md)
