{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Future(tests) where

import           Control.Lens                                    (traversed,to, (^..), view)
import           Text.PrettyPrint.Leijen.Text                    (indent, integer, comma, hsep, renderPretty, displayTStrict, Doc, angles, braces, dquotes, linebreak, parens, punctuate, space, squotes, textStrict, vsep, (<+>))
import           Control.Monad                                   (void)
import           Data.Either                                     (isRight)
import           Data.Foldable                                   (traverse_)
import qualified Data.Set                                        as Set
import Data.List (intersperse)
import           Data.Set                                        (Set)
import qualified Data.Map                                        as Map
import qualified Language.PlutusTx.AssocMap                      as AssocMap
import           Hedgehog                                        (assert, footnote, footnoteShow, Property, forAll, property)
import qualified Hedgehog                                        ()
import           Test.Tasty                                      (testGroup, TestTree)
import           Test.Tasty.Hedgehog                             (testProperty)
import qualified Test.Tasty.HUnit                                as HUnit

import qualified Spec.Lib                                        as Lib

import qualified Ledger
import qualified Data.Text                                       as Text
import           Ledger
import           Ledger.Ada                                      (Ada)
import qualified Ledger.Ada                                      as Ada
import           Ledger.Validation                               (OracleValue (..))
import qualified Ledger.Value                                    as Value
import           Ledger.Value                                    (Value(Value))
import           Prelude                                         hiding (init)
import qualified Wallet.Graph                                    as V
import           Wallet.API                                      (PubKey (..))
import           Wallet.Emulator
import qualified Wallet.Emulator.Generators                      as Gen
import qualified Wallet.Generators                               as Gen

import qualified Language.PlutusTx as PlutusTx

import           Language.PlutusTx.Coordination.Contracts.Future (Future (..), FutureData (..))
import qualified Language.PlutusTx.Coordination.Contracts.Future as F

-- | Wallet 1. Holder of the "long" position in the contract.
wallet1 :: Wallet
wallet1 = Wallet 1

-- | Wallet 2. Holder of the "short" position in the contract.
wallet2 :: Wallet
wallet2 = Wallet 2

tests :: TestTree
tests = testGroup "futures" [
    testProperty "commit initial margin" initialiseFuture,
    -- testProperty "close the position" settle,
    -- testProperty "close early if margin payment was missed" settleEarly,
    testProperty "increase the margin" increaseMargin,
    Lib.goldenPir "test/Spec/future.pir" $$(PlutusTx.compile [|| F.mkValidator ||]),
    HUnit.testCase "script size is reasonable" (Lib.reasonable (F.validatorScript contract) 50000)
    ]

init :: Wallet -> Trace MockWallet Ledger.TxOutRef
init w = outp <$> walletAction w (F.initialise (walletPubKey wallet1) (walletPubKey wallet2) contract) where
    outp = snd . head . filter (Ledger.isPayToScriptOut . fst) . Ledger.txOutRefs . head . snd

adjustMargin :: Wallet -> [Ledger.TxOutRef] -> FutureData -> Ada -> Trace MockWallet Ledger.TxOutRef
adjustMargin w refs fd vl =
    outp <$> walletAction w (F.adjustMargin refs contract fd vl) where
        outp = snd . head . filter (Ledger.isPayToScriptOut . fst) . Ledger.txOutRefs . head . snd

-- | Initialise the futures contract with contributions from wallets 1 and 2,
--   and update all wallets. Running `initBoth` will increase the slot number
--   by 2.
initBoth :: Trace MockWallet [Ledger.TxOutRef]
initBoth = do
    updateAll
    ins <- traverse init [wallet1, wallet2]
    updateAll
    return ins


initialiseFuture :: Property
initialiseFuture = checkTrace $ do
    void initBoth
    traverse_ (uncurry assertOwnFundsEq) [
        (wallet1, Value.minus startingBalance (Ada.toValue initMargin)),
        (wallet2, Value.minus startingBalance (Ada.toValue initMargin))]

settle :: Property
settle = checkTrace $ do
    ins <- initBoth
    let
        im = initMargin
        cur = FutureData (walletPubKey wallet1) (walletPubKey wallet2) im im
        spotPrice = 1124
        delta = fromIntegral units * (spotPrice - forwardPrice)
        ov  = OracleValue oracle (Ledger.Slot 10) spotPrice

    -- advance the clock to slot 10
    void $ addBlocks 8
    void $ walletAction wallet2 (F.settle ins contract cur ov)
    updateAll
    traverse_ (uncurry assertOwnFundsEq) [
        (wallet1, Value.plus  startingBalance (Ada.toValue delta)),
        (wallet2, Value.minus startingBalance (Ada.toValue delta))]

settleEarly :: Property
settleEarly = checkTrace $ do
    ins <- initBoth
    let
        im = initMargin
        cur = FutureData (walletPubKey wallet1) (walletPubKey wallet2) im im

        -- In this example, the price moves up (in favour of the long position)
        -- Wallet 2 fails to make the required margin payment, so wallet 1
        -- can the entire margin of wallet 2, including the penalty fee.
        -- This illustrates how the participants are incentivised to keep
        -- up with the variation margin.
        (_, upper) = marginRange
        spotPrice = upper + 1
        ov  = OracleValue oracle (Ledger.Slot 8) spotPrice

    -- advance the clock to slot 8
    void $ addBlocks 6
    void $ walletAction wallet1 (F.settleEarly ins contract cur ov)
    updateAll
    traverse_ (uncurry assertOwnFundsEq) [
        (wallet1, Value.plus  startingBalance (Ada.toValue initMargin)),
        (wallet2, Value.minus startingBalance (Ada.toValue initMargin))]

increaseMargin :: Property
increaseMargin = checkTrace $ do
    ins <- initBoth
    let
        im = initMargin
        cur = FutureData (walletPubKey wallet1) (walletPubKey wallet2) im im
        increase = fromIntegral units * 5

    -- advance the clock to slot 8
    void $ addBlocks 6

    -- Commit an additional `units * 5` amount of funds
    ins' <- adjustMargin wallet2 ins cur increase
    updateAll
    traverse_ (uncurry assertOwnFundsEq) [
        (wallet2, Value.minus startingBalance (Ada.toValue (initMargin + increase)))]
    -- advance the clock to slot 10
    void $ addBlocks 2

    -- Now the contract has ended successfully and wallet 2 gets some of its
    -- margin back.
    let
        im' = initMargin + increase
        cur' = cur { futureDataMarginShort = im' }

        (_, upper) = marginRange
        -- Note that this price would have caused the contract to end
        -- prematurely if it hadn't been for the increase (compare with
        -- settleEarly above)
        spotPrice = upper + 1

        delta = fromIntegral units * (spotPrice - forwardPrice)
        ov  = OracleValue oracle (Ledger.Slot 10) spotPrice

    void $ walletAction wallet2 (F.settle [ins'] contract cur' ov)
    updateAll

    -- NOTE: At this point, (initMargin - penalty) < delta < im'
    --       Meaning that it is still more profitable for wallet 2
    --       to see the contract through (via `settle`) than to
    --       simply ignore it and hence lose its entire margin im'.
    traverse_ (uncurry assertOwnFundsEq) [
        (wallet1, Value.plus  startingBalance (Ada.toValue delta)),
        (wallet2, Value.minus startingBalance (Ada.toValue delta))]

-- | A futures contract over 187 units with a forward price of 1233, due at
--   10 blocks.
contract :: Future
contract = Future {
    futureDeliveryDate  = Ledger.Slot 10,
    futureUnits         = units,
    futureUnitPrice     = forwardPrice,
    futureInitialMargin = im,
    futurePriceOracle   = oracle,
    futureMarginPenalty = penalty
    } where
        im = penalty + (Ada.fromInt units * forwardPrice `div` 20) -- 5%

-- | Margin penalty
penalty :: Ada
penalty = 1000

-- | The forward price agreed at the beginning of the contract.
forwardPrice :: Ada
forwardPrice = 1123

-- | Range within which the underlying asset's price can move before the first
--   margin payment is necessary.
--   If the price approaches the lower range, then the buyer (long position,
--   Wallet 1) has to increase their margin, and vice versa.
marginRange :: (Ada, Ada)
marginRange = (forwardPrice - delta, forwardPrice + delta) where
    delta = forwardPrice `div` 20

-- | How many units of the underlying asset are covered by the contract.
units :: Integer
units = 187

oracle :: PubKey
oracle = walletPubKey (Wallet 3)

initMargin :: Ada
initMargin = futureInitialMargin contract

-- | Funds available to wallets at the beginning.
startingBalance :: Ledger.Value
startingBalance = Ada.adaValueOf 1000000

-- | Run a trace with the given scenario and check that the emulator finished
--   successfully with an empty transaction pool.
checkTrace :: Trace MockWallet () -> Property
checkTrace t = property $ do
    let
        ib = Map.fromList [(walletPubKey wallet1, startingBalance), (walletPubKey wallet2, startingBalance)]
        model = Gen.generatorModel { Gen.gmInitialBalance = ib }
    (result, evaluation) <- forAll $ Gen.runTraceOn model t

    -- let blockchain = view chainNewestFirst evaluation
    -- let pubKeys :: [PubKey]
    --     pubKeys = evaluation ^.. (walletStates . traversed . ownPrivateKey . to Ledger.toPublicKey)
    -- let flowgraph = V.graph $ V.txnFlows pubKeys blockchain

    -- Hedgehog.footnoteShow flowgraph
    -- Hedgehog.footnote "FLOW"

    -- traverse_ Hedgehog.footnoteShow (view walletStates evaluation)
    -- Hedgehog.footnote "WALLETS"

    -- Hedgehog.footnoteShow (view index evaluation)
    -- Hedgehog.footnote "INDEX"

    -- traverse_ Hedgehog.footnoteShow (view emulatorLog evaluation)
    -- Hedgehog.footnote "LOG"

    Hedgehog.footnote . showChain $ view chainNewestFirst evaluation
    Hedgehog.footnote "CHAIN"

    Hedgehog.assert (isRight result)
    Hedgehog.assert ([] == _txPool evaluation)

-- | Validate all pending transactions and notify all wallets
updateAll :: Trace MockWallet ()
updateAll =
    processPending >>= void . walletsNotifyBlock [wallet1, wallet2]

showChain :: [[Tx]] -> String
showChain = Text.unpack . displayTStrict . renderPretty 0.8 200 . render

class Render a where
    render :: a -> Doc

instance Render [[Tx]] where
  render [] = "NO SLOTs"
  render slots = vsep (intersperse linebreak (render <$> slots))

instance Render [Tx] where
  render [] = "NO TXs"
  render txs = vsep (render <$> txs)

instance Render (Set TxIn) where
  render (Set.null -> True) = "No INPUTs"
  render txIns = vsep (render <$> (Set.toList txIns))

instance Render [TxOut] where
  render [] = "No OUTPUTs"
  render txOuts = vsep (render <$> txOuts)

instance Render TxIn where
    render t@(TxInOf {txInRef, txInType}) =
        inset $
        vsep $
        punctuate
            comma
            ["txInType" <+> showDoc txInType, "txInRef" <+> showDoc txInRef]

instance Render Address where
  render (AddressOf address) = textStrict . Text.pack . take 7 . show $ address

instance Render TxOut where
    render (TxOutOf {txOutAddress, txOutValue, txOutType}) =
        render txOutAddress <> ":" <+>
        inset
            (vsep
                 ([ "txOutType" <+> showDoc txOutType
                  ,  render txOutValue
                  ]))

instance Render Tx where
    render (Tx {txInputs, txOutputs, txForge, txFee, txValidRange, txSignatures}) =
        vsep
            [ "txForge" <+> render txForge
            , "txFee" <+> render txFee
            , "txInputs" <+> inset (render txInputs)
            , "txOutputs" <+> inset (render txOutputs)
            ]

instance Render Ada where
  render = render . Ada.toValue

instance Render Value where
  render (Value.isZero -> True) = "-NoValue-"
  render (Value val) = render val

instance (Render k, Render v) => Render (AssocMap.Map k v) where
    render m =
        vsep ((\(k, v) -> render k <+> parens (render v)) <$> AssocMap.toList m)

instance Render CurrencySymbol where
  render (Value.unCurrencySymbol -> "") = "ADA"
  render symbol = showDoc symbol

instance Render Value.TokenName where
  render (Value.unTokenName -> "") = "T"
  render symbol = showDoc symbol

instance Render Integer where
  render = integer

showDoc :: Show a => a -> Doc
showDoc = textStrict . Text.pack . show

            -- Tx {txInputs = [
            --        TxInOf {txInRef = TxOutRefOf {txOutRefId = TxIdOf {getTxId = 1b42dc18b99978b77a58ceb9623aed84a0d6e449a14873b0b5b08df1215e8f74}, txOutRefIdx = 0},
            --                txInType = ConsumePublicKeyAddress (PubKey {getPubKey = 3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c})}
            --     ],
            --     txOutputs = [
            --        TxOutOf {txOutAddress = AddressOf {getAddress = b566c4e4997c7473c95c01140b57c746b55e6c1ceb44993d8d784bfca590c4be},
            --                 txOutValue = Value {getValue = Map {unMap = [(, Map {unMap = [(, 11500)]})]}},
            --                 txOutType = PayToScript DataScript { <script> }
            --                },
            --        TxOutOf {txOutAddress = AddressOf {getAddress = 2721f657e9ed91d2fc2a282f7ff5ed81ae48f48b51061a0aa2faa4b25b5f0322},
            --                 txOutValue = Value {getValue = Map {unMap = [(, Map {unMap = [(, 988500)]})]}},
            --                 txOutType = PayToPubKey (PubKey {getPubKey = 3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c})}
            --     ],
            --     txForge = Value {getValue = Map {unMap = []}},
            --     txFee = Ada {getAda = 0},
            --     txValidRange = Interval {ivFrom = Nothing, ivTo = Nothing},
            --     txSignatures = fromList [(PubKey {getPubKey = 3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c}
            --                             , Signature {getSignature = "\242\140T\169-\178*\138\&3\145f\173\233\146x=\EM\248@\173\ACKH\195\238G\b\180(I\207\247\175\168\190\133\226\155N\236^\134b\204j\145\172H\211\176\&9x\234\180\193F\152\241d\141\t3\130\207\ACK"})
            --                             ]
            --    }


inset :: Doc -> Doc
inset doc = linebreak <> indent 4 doc
