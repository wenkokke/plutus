{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.DeepSeq              (NFData)
import           Criterion.Main
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as ASCII
import           Data.ByteString.Lazy.Hash
import           Math.NumberTheory.Logarithms

benchOp :: (NFData a) => (Integer -> Integer -> a) -> Integer -> Benchmark
benchOp op n = bench (show (integerLog10 n)) $ nf (`op` n) n

benchBSOp :: NFData a => (ByteString -> ByteString -> a) -> ByteString -> Benchmark
benchBSOp op n = bench (show (BSL.length n)) $ nf (`op` n) n

benchHash :: (ByteString -> ByteString) -> ByteString -> Benchmark
benchHash hash str = bench (show (BSL.length str)) $ nf hash str

integers :: [Integer]
integers = [ 1
           , 10 ^ (100 :: Int)
           , 10 ^ (10000 :: Int)
           ]

bytestrings :: [ByteString]
bytestrings = [ ASCII.replicate 1 'a'
              , ASCII.replicate 10 'a'
              , ASCII.replicate 100 'a'
              , ASCII.replicate 1000 'a'
              ]

main :: IO ()
main =
    defaultMain [ bgroup "Integer Addition" $
                      benchOp (+) <$> integers
                , bgroup "Integer Multiplication" $
                      benchOp (*) <$> integers
                , bgroup "Integer Subtraction" $
                      benchOp (-) <$> integers
                , bgroup "Integer Division" $
                      benchOp div <$> integers
                , bgroup "Integer Quotient" $
                      benchOp quot <$> integers
                , bgroup "Integer Remainder" $
                      benchOp rem <$> integers
                , bgroup "Integer Modulo" $
                      benchOp mod <$> integers
                , bgroup "Integer ≤" $
                      benchOp (<=) <$> integers
                , bgroup "Integer <" $
                      benchOp (<) <$> integers
                , bgroup "Integer >" $
                      benchOp (>) <$> integers
                , bgroup "Integer ≥" $
                      benchOp (>=) <$> integers
                , bgroup "Integer =" $
                      benchOp (==) <$> integers
                , bgroup "SHA256_2" $
                      benchHash sha2 <$> bytestrings
                , bgroup "SHA256_3" $
                      benchHash sha3 <$> bytestrings
                , bgroup "Bytestring =" $
                      benchBSOp (==) <$> bytestrings
                ]
