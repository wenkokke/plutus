{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GistSpec where

import qualified Bazel.Runfiles       as Runfiles
import qualified Control.Exception    as E
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            ()
import           Gist                 (Gist)
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec = gistJsonHandlingSpec

gistJsonHandlingSpec :: Spec
gistJsonHandlingSpec =
  describe "Gist JSON handling" $
  it "Should be able to parse a github response" $ do
    file <- dataFile
    input1 <- LBS.readFile file
    let decoded :: Either String [Gist] = eitherDecode input1
    length <$> decoded `shouldBe` Right 30

dataFile :: IO String
dataFile = do
    mr <-
        E.catch
            (Just <$> Runfiles.create)
            (\(_ :: E.SomeException) -> pure Nothing)
    pure $ case mr of
        Just r  -> Runfiles.rlocation r "plutus/plutus-playground/plutus-playground-server/test/gists1.json"
        Nothing -> "test/gists1.json"
