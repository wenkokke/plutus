{-# LANGUAGE FlexibleContexts #-}

module Playground.Interpreter.Util where

import           Data.Aeson                 (FromJSON, Result (Success), ToJSON, fromJSON)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.Typeable              as T
import           Wallet.Emulator.Types      (AssertionError, EmulatedWalletApi, EmulatorState (emChain), Trace,
                                             Wallet (Wallet), runTraceChain, runTraceTxPool)
import           Wallet.Generators          (GeneratorModel (GeneratorModel))
import qualified Wallet.Generators          as Gen
import           Wallet.UTXO                (Blockchain, PubKey (PubKey), Tx, Value (Value))

runTrace ::
     [(Wallet, Integer)]
  -> Trace EmulatedWalletApi [Tx]
  -> Either AssertionError Blockchain
runTrace wallets action =
  let walletToBalance (Wallet i, v) = (PubKey i, Value v)
      initialBalance = Map.fromList $ fmap walletToBalance wallets
      pubKeys = Set.fromList $ fmap (\(Wallet i, _) -> PubKey i) wallets
      (initialTx, _) =
        Gen.genInitialTransaction $ GeneratorModel initialBalance pubKeys
      (eRes, newState) = runTraceTxPool [initialTx] action
   in case eRes of
        Right _ -> Right . emChain $ newState
        Left e  -> Left e

-- | This will throw an exception if it cannot decode the json however it should
--   never do this as long as it is only called in places where we have already
--   decoded and encoded the value since it came from an HTTP API call
{-# ANN decode ("HLint: ignore" :: String) #-}

decode :: (FromJSON a, T.Typeable a) => String -> a
decode v =
  let x = JSON.eitherDecode . BSL.pack $ v
   in case x of
        Right a -> a
        Left e ->
          error $
          "couldn't decode " ++
          v ++ " :: " ++ show (T.typeOf x) ++ " (" ++ show e ++ ")"

apply1 :: (T.Typeable a, FromJSON a) => (a -> b) -> String -> b
apply1 fun v = fun (decode v)

apply2 ::
     (T.Typeable a, FromJSON a, T.Typeable b, FromJSON b)
  => (a -> b -> c)
  -> String
  -> String
  -> c
apply2 fun a b = fun (decode a) (decode b)

apply3 ::
     ( T.Typeable a
     , T.Typeable a
     , FromJSON a
     , T.Typeable b
     , FromJSON b
     , T.Typeable c
     , FromJSON c
     )
  => (a -> b -> c -> d)
  -> String
  -> String
  -> String
  -> d
apply3 fun a b c = fun (decode a) (decode b) (decode c)

apply4 ::
     ( T.Typeable a
     , FromJSON a
     , T.Typeable b
     , FromJSON b
     , T.Typeable c
     , FromJSON c
     , T.Typeable d
     , FromJSON d
     )
  => (a -> b -> c -> d -> e)
  -> String
  -> String
  -> String
  -> String
  -> e
apply4 fun a b c d = fun (decode a) (decode b) (decode c) (decode d)

apply5 ::
     ( T.Typeable a
     , FromJSON a
     , T.Typeable b
     , FromJSON b
     , T.Typeable c
     , FromJSON c
     , T.Typeable d
     , FromJSON d
     , T.Typeable e
     , FromJSON e
     )
  => (a -> b -> c -> d -> e -> f)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> f
apply5 fun a b c d e =
  fun (decode a) (decode b) (decode c) (decode d) (decode e)

apply6 ::
     ( T.Typeable a
     , FromJSON a
     , T.Typeable b
     , FromJSON b
     , T.Typeable c
     , FromJSON c
     , T.Typeable d
     , FromJSON d
     , T.Typeable e
     , FromJSON e
     , T.Typeable f
     , FromJSON f
     )
  => (a -> b -> c -> d -> e -> f -> g)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> g
apply6 fun a b c d e f =
  fun (decode a) (decode b) (decode c) (decode d) (decode e) (decode f)

apply7 ::
     ( T.Typeable a
     , FromJSON a
     , T.Typeable b
     , FromJSON b
     , T.Typeable c
     , FromJSON c
     , T.Typeable d
     , FromJSON d
     , T.Typeable e
     , FromJSON e
     , T.Typeable f
     , FromJSON f
     , T.Typeable g
     , FromJSON g
     )
  => (a -> b -> c -> d -> e -> f -> g -> h)
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> h
apply7 fun a b c d e f g =
  fun
    (decode a)
    (decode b)
    (decode c)
    (decode d)
    (decode e)
    (decode f)
    (decode g)
