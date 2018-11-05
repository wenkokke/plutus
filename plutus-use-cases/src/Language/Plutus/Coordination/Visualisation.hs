module Language.Plutus.Coordination.Visualisation(
    module Wallet.Visualisation,
    -- * Traces for crowdfunding
    cwdCommit,
    cwdSuccess,
    cwdRefund,
    -- * Traces for future
    ftSuccess,
    ftSettleEarly,
    ftIncreaseMargin
    ) where

import           Control.Monad                                       (void)
import           Wallet.Emulator
import           Wallet.UTXO.Runtime                                 (OracleValue (..), Signed (..))
import qualified Wallet.UTXO.Runtime                                 as Runtime
import qualified Wallet.UTXO.Types                                   as UTXO
import           Wallet.Visualisation

import           Language.Plutus.Coordination.Contracts.CrowdFunding (Campaign (..), contribute, refund)
import qualified Language.Plutus.Coordination.Contracts.CrowdFunding as CF
import           Language.Plutus.Coordination.Contracts.Future       (Future (..), FutureData (..))
import qualified Language.Plutus.Coordination.Contracts.Future       as F


--- CROWDFUNDING DEFINITIONS

campaign :: Campaign
campaign = Campaign {
    campaignDeadline = 10,
    campaignTarget   = 10000,
    campaignCollectionDeadline = 15,
    campaignOwner              = PubKey 1
    }

cwdCommit :: Trace EmulatedWalletApi ()
cwdCommit = do
    _ <- contrib (Wallet 2) 6000
    _ <- contrib (Wallet 3) 8000
    updateAll

cwdSuccess :: Trace EmulatedWalletApi ()
cwdSuccess = do
    con2 <- contrib (Wallet 2) 6000
    con3 <- contrib (Wallet 3) 8000
    updateAll
    addBlocks 10
    _ <- collect w1 [(con2, 6000), (con3, 8000)]
    updateAll

cwdRefund :: Trace EmulatedWalletApi ()
cwdRefund = do
    con2 <- contrib (Wallet 2) 6000
    con3 <- contrib (Wallet 3) 8000
    updateAll
    addBlocks 15
    refund' w2 con2 6000
    refund' w3 con3 8000
    updateAll


-- Future definitions

-- | A futures contract over 187 units with a forward price of 1233, due at
--   10 blocks.
contract :: Future
contract = Future {
    futureDeliveryDate  = 10,
    futureUnits         = units,
    futureUnitPrice     = forwardPrice,
    futureInitialMargin = im,
    futurePriceOracle   = oracle,
    futureMarginPenalty = penalty
    } where
        im = penalty + (units * forwardPrice `div` 20) -- 5%


ftSuccess :: Trace EmulatedWalletApi ()
ftSuccess = do
    ins <- ftInitBoth

    -- advance the clock to block height 10
    void $ addBlocks 8

    -- get the current (spot) price from the oracle
    let
        spotPrice = 1124
        ov  = OracleValue (Signed (oracle, (10, spotPrice)))

    -- current state of the contract
    let 
        im = fromIntegral initMargin
        cur = FutureData (PubKey 1) (PubKey 2) im im

    void $ walletAction w2 (F.settle ins contract cur ov)
    updateAll

ftSettleEarly :: Trace EmulatedWalletApi ()
ftSettleEarly = do
    ins <- ftInitBoth
    
    let
        -- In this example, the price moves up (in favour of the long position)
        -- Wallet 2 fails to make the required margin payment, so wallet 1
        -- can the entire margin of wallet 2, including the penalty fee.
        -- This illustrates how the participants are incentivised to keep
        -- up with the variation margin.
        (_, upper) = marginRange
        spotPrice = upper + 1
        ov  = OracleValue (Signed (oracle, (8, spotPrice)))
        im = fromIntegral initMargin
        cur = FutureData (PubKey 1) (PubKey 2) im im
    
    -- advance the clock to block height 8
    void $ addBlocks 6

    void $ walletAction w1 (F.settleEarly ins contract cur ov)
    updateAll

ftIncreaseMargin :: Trace EmulatedWalletApi ()
ftIncreaseMargin = do
    ins <- ftInitBoth
    let
        im = fromIntegral initMargin
        cur = FutureData (PubKey 1) (PubKey 2) im im
        increase = fromIntegral units * 5

    -- advance the clock to block height 8
    void $ addBlocks 6

    -- Commit an additional `units * 5` amount of funds
    ins' <- ftAdjustMargin w2 ins cur increase
    updateAll

    -- advance the clock
    void $ addBlocks 2

    -- Now the contract has ended successfully and wallet 2 gets some of its
    -- margin back.
    let
        im' = fromIntegral $ initMargin + increase
        cur' = cur { futureDataMarginShort = im' }

        (_, upper) = marginRange
        -- Note that this price would have caused the contract to end
        -- prematurely if it hadn't been for the increase (compare with
        -- settleEarly above)
        spotPrice = upper + 1

        delta = fromIntegral $ units * (spotPrice - forwardPrice)
        ov  = OracleValue (Signed (oracle, (10, spotPrice)))

    void $ walletAction w2 (F.settle [ins'] contract cur' ov)
    updateAll

-- | Margin penalty
penalty :: Runtime.Value
penalty = 1000

-- | The forward price agreed at the beginning of the contract.
forwardPrice :: Runtime.Value
forwardPrice = 1123

-- | Range within which the underlying asset's price can move before the first
--   margin payment is necessary.
--   If the price approaches the lower range, then the buyer (long position,
--   Wallet 1) has to increase their margin, and vice versa.
marginRange :: (Runtime.Value, Runtime.Value)
marginRange = (forwardPrice - delta, forwardPrice + delta) where
    delta = forwardPrice `div` 20

-- | How many units of the underlying asset are covered by the contract.
units :: Int
units = 187

oracle :: PubKey
oracle = PubKey 17

initMargin :: UTXO.Value
initMargin = fromIntegral $ futureInitialMargin contract


--- CONVENIENCES
---  DO NOT LOOK HERE

-- | Make a contribution to the campaign from a wallet. Returns the reference
--   to the transaction output that is locked by the campaign's validator
--   script (and can be collected by the campaign owner)
contrib :: Wallet -> Runtime.Value -> Trace EmulatedWalletApi  TxOutRef'
contrib w v = exContrib <$> walletAction w (contribute campaign v) where
    exContrib = snd . head . filter (isPayToScriptOut . fst) . txOutRefs . head

-- | Collect the contributions of a crowdfunding campaign
collect :: Wallet -> [(TxOutRef', UTXO.Value)] -> Trace EmulatedWalletApi  [Tx]
collect w = walletAction w . CF.collect campaign

refund' :: Wallet -> TxOutRef' -> UTXO.Value -> Trace EmulatedWalletApi ()
refund' wl rf vl = void $ walletAction wl (refund campaign rf vl)

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

-- | Initialise the futures contract with contributions from wallets 1 and 2,
--   and update all wallets. Running `initBoth` will increase the block height
--   by 2.
ftInitBoth :: Trace EmulatedWalletApi [TxOutRef']
ftInitBoth = do
    updateAll
    ins <- traverse ftInit [w1, w2]
    updateAll
    return ins


ftInit :: Wallet -> Trace EmulatedWalletApi TxOutRef'
ftInit w = outp <$> walletAction w (F.initialise (PubKey 1) (PubKey 2) contract) where
    outp = snd . head . filter (isPayToScriptOut . fst) . txOutRefs . head

ftAdjustMargin :: Wallet -> [TxOutRef'] -> FutureData -> UTXO.Value -> Trace EmulatedWalletApi TxOutRef'
ftAdjustMargin w refs fd vl =
    outp <$> walletAction w (F.adjustMargin refs contract fd vl) where
        outp = snd . head . filter (isPayToScriptOut . fst) . txOutRefs . head