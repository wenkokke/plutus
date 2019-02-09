{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.DeepSeq              (NFData)
import           Criterion.Main
import           Crypto.ECC.Ed25519Donna
import           Crypto.Error
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as ASCII
import           Data.ByteString.Lazy.Hash
import           Data.Word                    (Word8)
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

hexParse :: BSL.ByteString -> BSL.ByteString
hexParse = BSL.pack . asBytes . BSL.unpack
    where
        asBytes :: [Word8] -> [Word8]
        asBytes []        = mempty
        asBytes (c:c':cs) = handlePair c c' : asBytes cs
        asBytes _         = undefined --

        -- Turns a pair of bytes written in hexadecimal such as @a6@ into a single Word8
        handlePair :: Word8 -> Word8 -> Word8
        handlePair c c' = 16 * handleChar c + handleChar c'

        handleChar :: Word8 -> Word8
        handleChar x
            | x >= 48 && x <= 57 = x - 48 -- hexits 0-9
            | x >= 97 && x <= 102 = x - 87 -- hexits a-f
            | otherwise = error "Must be a hexit"

-- sample data from first line of https://ed25519.cr.yp.to/python/sign.input
-- see https://ed25519.cr.yp.to/python/sign.py for how to read input data


sampleSig :: BSL.ByteString
sampleSig = hexParse "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"

samplePubKey :: BSL.ByteString
samplePubKey = hexParse "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"

sampleMsg :: BSL.ByteString
sampleMsg = mempty

packSig :: BSL.ByteString -> CryptoFailable Signature
packSig = signature . BSL.toStrict

packPubKey :: BSL.ByteString -> CryptoFailable PublicKey
packPubKey = publicKey . BSL.toStrict

signatureByteString :: BSL.ByteString -- ^ Public key
                    -> BSL.ByteString -- ^ Message
                    -> BSL.ByteString -- ^ Signature
                    -> Maybe Bool
signatureByteString pubKey msg sig = maybeCryptoError (verify <$> packPubKey pubKey <*> pure (BSL.toStrict msg) <*> packSig sig)

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
                , bgroup "Ed25519"
                      [ bench "0" $ nf (signatureByteString samplePubKey sampleMsg) sampleSig ]
                ]
