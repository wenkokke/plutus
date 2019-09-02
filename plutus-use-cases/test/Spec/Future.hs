{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Spec.Future(tests) where

import           Test.Tasty
import qualified Test.Tasty.HUnit                                as HUnit

import qualified Spec.Lib                                        as Lib

import           Language.PlutusTx.Lattice
import qualified Ledger
import           Ledger.Ada                                      (Ada)
import qualified Ledger.Ada                                      as Ada
import           Ledger.Crypto                                   (PubKey (..))
import           Ledger.Validation                               (OracleValue (..))

import           Language.Plutus.Contract.Effects.ExposeEndpoint (HasEndpoint)
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Trace                  as Trace
import qualified Language.PlutusTx                               as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.Future (Future (..), FutureSchema)
import qualified Language.PlutusTx.Coordination.Contracts.Future as F

-- | Wallet 1. Holder of the "long" position in the contract.
wallet1 :: Wallet
wallet1 = Wallet 1

-- | Wallet 2. Holder of the "short" position in the contract.
wallet2 :: Wallet
wallet2 = Wallet 2

-- | Wallet 3. Initialises the contract.
wallet3 :: Wallet
wallet3 = Wallet 3

tests :: TestTree
tests =
    let con = F.runContract contract (walletPubKey wallet1) (walletPubKey wallet2) in
    testGroup "futures"
    [ checkPredicate "initialise future"
        con
        (walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        $ Trace.callEndpoint @"initialise" wallet3 ()
            >> Trace.handleBlockchainEvents wallet3

    , checkPredicate "close the position"
        con
        (walletFundsChange wallet1 (Ada.toValue $ initMargin + delta 1124)
            /\ walletFundsChange wallet2 (Ada.toValue $ initMargin - delta 1124)
            /\ walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        (settleTrace @"settle" 1124 8)

    , checkPredicate "close early if margin payment was missed"
        con
        (walletFundsChange wallet1 (Ada.toValue $ 2 * initMargin)
            /\ walletFundsChange wallet2 mempty
            /\ walletFundsChange wallet3 (Ada.toValue $ negate (2 * initMargin)))
        (let (_, upper) = marginRange
             spotPrice = upper + 1
        in settleTrace @"settle early" spotPrice 2)

    , checkPredicate "increase the margin"
        con
        (walletFundsChange wallet1 (Ada.lovelaceValueOf (-100)))
        (initialiseTrace
            >> Trace.callEndpoint @"adjust margin" wallet1 (Ada.lovelaceOf 100)
            >> Trace.handleBlockchainEvents wallet1)

    , Lib.goldenPir "test/Spec/future.pir" $$(PlutusTx.compile [|| F.mkValidator ||])
    , HUnit.testCase "script size is reasonable" (Lib.reasonable (F.validatorScript contract) 50000)
    ]

-- | Start the contract with wallet 1 as the short position, wallet 2 as the long
--   position, and wallet 3 as the wallet that makes the initial transaction (see note
--   "Initialising the futures contract".)
initialiseTrace
    :: ( MonadEmulator m )
    => ContractTrace FutureSchema m a ()
initialiseTrace = do
    Trace.callEndpoint @"start" wallet1 F.Long
    Trace.notifyInterestingAddresses wallet1
    Trace.callEndpoint @"start" wallet2  F.Short
    Trace.notifyInterestingAddresses wallet2
    Trace.callEndpoint @"initialise" wallet3 ()
    Trace.handleBlockchainEvents wallet3

-- | Settle the contract with the given spot price, after adding some
--   blocks and using the endpoint provided.
--   Note the 'HasEndpoint' constraint - this means we can instantiate 'l'
--   with "settle" and "settle early".
settleTrace
    :: forall l m a.
       ( MonadEmulator m 
       , HasEndpoint l (OracleValue Ada) FutureSchema
       )
    => Ada
    -- ^ Spot price
    -> Integer
    -- ^ How many blocks to add
    -> ContractTrace FutureSchema m a ()
settleTrace spotPrice blocks = do
    initialiseTrace
    Trace.addBlocks blocks
    let ov = OracleValue oracle (Ledger.Slot (blocks + 2)) spotPrice
    Trace.callEndpoint @l wallet1 ov
    Trace.handleBlockchainEvents wallet1

delta :: Ada -> Ada
delta spotPrice = fromIntegral units * (spotPrice - forwardPrice)

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
        im = penalty + (Ada.lovelaceOf units * forwardPrice `div` 20) -- 5%

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
marginRange = (forwardPrice - limit, forwardPrice + limit) where
    limit = forwardPrice `div` 20

-- | How many units of the underlying asset are covered by the contract.
units :: Integer
units = 187

oracle :: PubKey
oracle = walletPubKey (Wallet 4)

initMargin :: Ada
initMargin = futureInitialMargin contract
