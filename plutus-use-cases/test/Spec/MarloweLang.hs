{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-unused-do-bind #-}
module Spec.MarloweLang(tests) where

import           Data.Either                                         (isRight)
import           Control.Monad                                        (void)
import qualified Data.Map                                            as Map
import           Hedgehog                                            (Property, forAll, property)
import qualified Hedgehog
import           Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden as G
import           Test.Tasty.Hedgehog                                 (testProperty, HedgehogTestLimit(..))

import Ledger hiding (Value)
import qualified Ledger
import Ledger.Validation (OracleValue(..))
import qualified Ledger.Validation                                as Validation
import           Wallet                                           (PubKey (..))
import           Wallet.Emulator
import qualified Wallet.Generators                                as Gen
import qualified Language.PlutusTx
import           Language.Marlowe.Compiler

import           Language.Marlowe.Parser
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding as E
import Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Megaparsec as Megaparsec

import qualified Language.Marlowe.Escrow                            as Escrow

newtype MarloweScenario = MarloweScenario { mlInitialBalances :: Map.Map PubKey Ledger.Value }

tests :: TestTree
tests = testGroup "Marlowe" [
        testCase "commit/pay/redeem" $
            parseMarlowe (T.unlines ["contract Simple {",
                "0x1 commits 123 Ada before block 128 timeout block 256 as MyCommit {",
                    "0x1 pays 0x2 (100 Ada + 23 Ada) before block 256",
                "} else redeem MyCommit",
                "}"])
                @?= Right [CommitCash (IdentCC 0) (PubKey 1) (Value 123) 128 256
                    (Pay (IdentPay 1) (PubKey 1) (PubKey 2) (AddValue (Value 100) (Value 23)) 256 Null)
                    (RedeemCC (IdentCC 0) Null)],
        testCase "when" $
            parseMarlowe (T.unlines ["contract Simple {",
                "when not true and false or 20 Ada >= 2 Ada * 3 Ada + 1 Ada and now < block 1000 before block 128 then end",
                "}"])
                @?= Right [When
                    (OrObs
                        (AndObs (NotObs TrueObs) FalseObs)
                        (AndObs
                            (ValueGE (Value 20) (AddValue (MulValue (Value 2) (Value 3)) (Value 1)))
                            (BelowTimeout 1000)))
                    128 Null Null],
        testCase "Escrow" $
            parseMarlowe (T.unlines ["contract Escrow {",
                "0x1 commits 450 Ada before block 10 timeout block 100 as escrow {",
                    "when 0x1 chose and (0x2 chose or 0x3 chose) before block 90 then {",
                        "if (0x1 chose 1 and (0x2 chose 1 or 0x3 chose 1)) or (0x2 chose 1 and 0x3 chose 1) then {",
                            "0x1 pays 0x2 450 Ada before block 100",
                "} else redeem escrow } else redeem escrow }}"])
                @?= Right [CommitCash (IdentCC 0) (PubKey {getPubKey = 1}) (Value 450) 10 100 (When (AndObs (PersonChoseSomething (IdentChoice 1) (PubKey {getPubKey = 1})) (OrObs (PersonChoseSomething (IdentChoice 2) (PubKey {getPubKey = 2})) (PersonChoseSomething (IdentChoice 3) (PubKey {getPubKey = 3})))) 90 (Choice (OrObs (AndObs (PersonChoseThis (IdentChoice 4) (PubKey {getPubKey = 1}) 1) (OrObs (PersonChoseThis (IdentChoice 5) (PubKey {getPubKey = 2}) 1) (PersonChoseThis (IdentChoice 6) (PubKey {getPubKey = 3}) 1))) (AndObs (PersonChoseThis (IdentChoice 7) (PubKey {getPubKey = 2}) 1) (PersonChoseThis (IdentChoice 8) (PubKey {getPubKey = 3}) 1))) (Pay (IdentPay 1) (PubKey {getPubKey = 1}) (PubKey {getPubKey = 2}) (Value 450) 100 Null) (RedeemCC (IdentCC 0) Null)) (RedeemCC (IdentCC 0) Null)) Null],
        marlowe "Simple",
        marlowe "Escrow"
        ]

marlowe name = goldenVsString name (name ++ ".golden") $ do
    p <- TIO.readFile (name ++ ".marlowe")
    case parseMarlowe p of
        Right m -> do
            let result = show m
            let bs = C8.pack result
            return (LBS.fromStrict bs)
        Left err -> error $ Megaparsec.parseErrorPretty err



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

getScriptOutFromTx :: Tx -> (TxOut', TxOutRef')
getScriptOutFromTx tx = head . filter (isPayToScriptOut . fst) . txOutRefs $ tx

perform :: Wallet -> m () -> Trace m (TxOut', TxOutRef')
perform actor action = do
    [tx] <- walletAction actor action
    processPending >>= void . walletsNotifyBlock [actor]
    assertIsValidated tx
    return $ getScriptOutFromTx tx

withContract
    :: [Wallet]
    -> Contract
    -> ((TxOut', TxOutRef') -> Trace MockWallet ((TxOut', TxOutRef'), State))
    -> Trace MockWallet ()
withContract wallets contract f = do
    [tx] <- walletAction creator (createContract contract 12)
    let txOut = getScriptOutFromTx tx
    update
    assertIsValidated tx

    (tx1Out, state) <- f txOut

    [tx] <- walletAction creator (endContract tx1Out state)
    update
    assertIsValidated tx
  where
    creator = head wallets
    update  = updateAll wallets

