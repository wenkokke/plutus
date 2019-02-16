-- | Functions related to @integer@.

{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.StdLib.Data.Integer
    ( succInteger
    ) where

import           Language.PlutusCore.Constant.Make (makeDynBuiltinIntSizedAs)
import           Language.PlutusCore.MkPlc
import           Language.PlutusCore.Name
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Type

-- |  @succ :: Integer -> Integer@ as a PLC term.
--
-- > /\(s :: size) -> \(i : integer s) ->
-- >     addInteger {s} i (resizeInteger {1} {s} (sizeOfInteger {s} i) 1!1)
succInteger :: Term TyName Name ()
succInteger = runQuote $ do
    s <- freshTyName () "s"
    i  <- freshName () "i"
    return
        . TyAbs () s (Size ())
        . LamAbs () i (TyApp () (TyBuiltin () TyInteger) $ TyVar () s)
        . mkIterApp () (TyInst () (Builtin () $ BuiltinName () AddInteger) $ TyVar () s)
        $ [ Var () i
          , makeDynBuiltinIntSizedAs (TyVar () s) (Var () i) 1
          ]