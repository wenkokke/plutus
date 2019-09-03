{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

-- import qualified Spec.Actus
import qualified Spec.Marlowe3.Marlowe
import Language.Marlowe3.Common

import           Wallet.Emulator
import           Wallet.API
import           Test.Tasty
import           Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 30)

tests :: TestTree
tests = localOption limit $ testGroup "Marlowe Contracts"
        [ Spec.Marlowe3.Marlowe.tests
        ]
w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

key1, key2, key3 :: PubKey
key1 = walletPubKey w1
key2 = walletPubKey w2
key3 = walletPubKey w3

zeroCouponBond = When [ (Deposit acc key1 (Constant 850),
        Pay acc (Party key2) (Constant 850)
            (When
                [ (Deposit acc key2 (Constant 1000),
                    Pay acc (Party key1) (Constant 1000) Refund)
                ]
                1579305589
                Refund
            ))] 1563407989 Refund
  where
    acc = AccountId 1 key1