{-|
Description: PLC Syntax, typechecker,semantics property based testing.

This file contains
1. A duplicate of the Plutus Core Abstract Syntax (types and terms)
2. A kind checker and a type checker
3. Reduction semantics for types
-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Language.PlutusCore.Generators.NEAT.Type
  ( TypeBuiltinG (..)
  , TypeG (..)
  , ClosedTypeG
  , convertClosedType
  , TermG (..)
  , ClosedTermG
  , convertClosedTerm
  , Check (..)
  , stepTypeG
  , normalizeTypeG
  , GenError (..)
  , Neutral (..)
  ) where

import           Control.Enumerable
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Coolean (Cool,toCool,true,false,(&&&))
import qualified Data.Stream                    as Stream
import qualified Data.Text                      as Text
import           Language.PlutusCore
import           Language.PlutusCore.Generators.NEAT.Common
import           Text.Printf


-- * Helper definitions

newtype Neutral a = Neutral
  { unNeutral :: a
  }


-- * Enumeration

-- ** Enumerating types

data TypeBuiltinG
  = TyByteStringG
  | TyIntegerG
  | TyStringG
  deriving (Typeable, Eq, Show)

deriveEnumerable ''TypeBuiltinG

-- NOTE: Unusually, the application case is annotated with a kind.
--       The reason is eagerness and efficiency. If we have the kind
--       information at the application site, we can check the two
--       subterms in parallel, while evaluating as little as possible.

data TypeG tyname
  = TyVarG tyname
  | TyFunG (TypeG tyname) (TypeG tyname)
  | TyIFixG (TypeG tyname) (Kind ()) (TypeG tyname)
  | TyForallG (Kind ()) (TypeG (S tyname))
  | TyBuiltinG TypeBuiltinG
  | TyLamG (TypeG (S tyname))
  | TyAppG (TypeG tyname) (TypeG tyname) (Kind ())
  deriving (Typeable, Eq, Show, Functor)

deriveEnumerable ''Kind

deriveEnumerable ''TypeG

type ClosedTypeG = TypeG Z

instance Enumerable tyname => Enumerable (Normalized (TypeG tyname)) where
  enumerate = share $ aconcat
    [       c1 $ \ty -> Normalized (unNeutral ty)
    , pay . c1 $ \ty -> Normalized (TyLamG (unNormalized ty))
    ]

instance Enumerable tyname => Enumerable (Neutral (TypeG tyname)) where
  enumerate = share $ aconcat
    [ pay . c1 $ \i         -> Neutral (TyVarG i)
    , pay . c2 $ \ty1 ty2   -> Neutral (TyFunG (unNeutral ty1) (unNeutral ty2))
    , pay . c3 $ \ty1 k ty2 -> Neutral (TyIFixG (unNeutral ty1) k (unNeutral ty2))
    , pay . c2 $ \k ty      -> Neutral (TyForallG k (unNeutral ty))
    , pay . c1 $ \tyBuiltin -> Neutral (TyBuiltinG tyBuiltin)
    , pay . c3 $ \ty1 ty2 k -> Neutral (TyAppG (unNeutral ty1) (unNormalized ty2) k)
    ]


-- ** Enumerating terms

data TermG tyname name
    = VarG
      name
    | LamAbsG
      (TermG tyname (S name))
    | ApplyG
      (TermG tyname name)
      (TermG tyname name)
      (Normalized (TypeG tyname))
    | TyAbsG
      (TermG (S tyname) name)
    | TyInstG
      (TermG tyname name)
      (Normalized (TypeG (S tyname)))
      (Normalized (TypeG tyname))
      (Kind ())
    deriving (Typeable, Eq, Show)

instance Bifunctor TermG where
  bimap _ g (VarG i) = VarG (g i)
  bimap f g (TyAbsG tm) = TyAbsG (bimap (fmap f) g tm)
  bimap f g (LamAbsG tm) = LamAbsG (bimap f (fmap g) tm)
  bimap f g (ApplyG tm1 tm2 ty) = ApplyG (bimap f g tm1) (bimap f g tm2) (fmap (fmap f) ty)
  bimap f g (TyInstG tm vCod ty k) =
    TyInstG (bimap f g tm) (fmap (fmap (fmap f)) vCod) (fmap (fmap f) ty) k

deriveEnumerable ''StaticBuiltinName

deriveEnumerable ''TermG

type ClosedTermG = TermG Z Z


-- * Converting types

-- |Convert generated builtin types to Plutus builtin types.
convertTypeBuiltin :: TypeBuiltinG -> Some (TypeIn DefaultUni)
convertTypeBuiltin TyByteStringG = Some (TypeIn DefaultUniByteString)
convertTypeBuiltin TyIntegerG    = Some (TypeIn DefaultUniInteger)
convertTypeBuiltin TyStringG     = Some (TypeIn DefaultUniString)

-- |Convert well-kinded generated types to Plutus types.
--
-- NOTE: Passes an explicit `TyNameState`, instead of using a State monad,
--       as the type of the `TyNameState` changes throughout the computation.
--       Alternatively, this could be written using an indexed State monad.
convertType
  :: (Show tyname, MonadQuote m, MonadError GenError m)
  => TyNameState tyname -- ^ Type name environment with fresh name stream
  -> Kind ()            -- ^ Kind of type below
  -> TypeG tyname       -- ^ Type to convert
  -> m (Type TyName DefaultUni ())
convertType tns _ (TyVarG i) =
  return (TyVar () (tynameOf tns i))
convertType tns (Type _) (TyFunG ty1 ty2) =
  TyFun () <$> convertType tns (Type ()) ty1 <*> convertType tns (Type ()) ty2
convertType tns (Type _) (TyIFixG ty1 k ty2) =
  TyIFix () <$> convertType tns k' ty1 <*> convertType tns k ty2
  where
    k' = KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))
convertType tns (Type _) (TyForallG k ty) = do
  tns' <- extTyNameState tns
  TyForall () (tynameOf tns' FZ) k <$> convertType tns' (Type ()) ty
convertType _ _ (TyBuiltinG tyBuiltin) =
  return (TyBuiltin () (convertTypeBuiltin tyBuiltin))
convertType tns (KindArrow _ k1 k2) (TyLamG ty) = do
  tns' <- extTyNameState tns
  TyLam () (tynameOf tns' FZ) k1 <$> convertType tns' k2 ty
convertType tns k2 (TyAppG ty1 ty2 k1) =
  TyApp () <$> convertType tns (KindArrow () k1 k2) ty1 <*> convertType tns k1 ty2
convertType _ k ty = throwError $ BadTypeG k ty

-- |Convert generated closed types to Plutus types.
convertClosedType
  :: (MonadQuote m, MonadError GenError m)
  => Stream.Stream Text.Text
  -> Kind ()
  -> ClosedTypeG
  -> m (Type TyName DefaultUni ())
convertClosedType tynames = convertType (emptyTyNameState tynames)


-- ** Converting terms

-- |Convert well-kinded generated types to Plutus types.
--
-- NOTE: Passes an explicit `TyNameState` and `NameState`, instead of using a
--       State monad, as the type of the `TyNameState` changes throughout the
--       computation. This could be written using an indexed State monad.
convertTerm
  :: (Show tyname, Show name, MonadQuote m, MonadError GenError m)
  => TyNameState tyname -- ^ Type name environment with fresh name stream
  -> NameState name     -- ^ Name environment with fresh name stream
  -> TypeG tyname       -- ^ Type of term below
  -> TermG tyname name  -- ^ Term to convert
  -> m (Term TyName Name DefaultUni ())
convertTerm _tns ns _ty (VarG i) =
  return (Var () (nameOf ns i))
convertTerm tns ns (TyFunG ty1 ty2) (LamAbsG tm) = do
  ns' <- extNameState ns
  ty1' <- convertType tns (Type ()) ty1
  LamAbs () (nameOf ns' FZ) ty1' <$> convertTerm tns ns' ty2 tm
convertTerm tns ns ty2 (ApplyG tm1 tm2 (Normalized ty1)) =
  Apply () <$> convertTerm tns ns (TyFunG ty1 ty2) tm1 <*> convertTerm tns ns ty1 tm2
convertTerm tns ns (TyForallG k ty) (TyAbsG tm) = do
  tns' <- extTyNameState tns
  TyAbs () (tynameOf tns' FZ) k <$> convertTerm tns' ns ty tm
convertTerm tns ns _ (TyInstG tm (Normalized cod) (Normalized ty) k) =
  TyInst () <$> convertTerm tns ns (TyForallG k cod) tm <*> convertType tns k ty
convertTerm _ _ ty tm = throwError $ BadTermG ty tm

-- |Convert generated closed terms to Plutus terms.
convertClosedTerm
  :: (MonadQuote m, MonadError GenError m)
  => Stream.Stream Text.Text
  -> Stream.Stream Text.Text
  -> ClosedTypeG
  -> ClosedTermG
  -> m (Term TyName Name DefaultUni ())
convertClosedTerm tynames names = convertTerm (emptyTyNameState tynames) (emptyNameState names)


-- * Checking

class Check t a where
  check :: t -> a -> Cool


-- ** Kind checking

-- |Kind check builtin types.
--
-- NOTE: If we make |checkTypeBuiltinG| non-strict in its second argument,
--       lazy-search will only ever return one of the various builtin types.
--       Perhaps this is preferable?
--
instance Check (Kind ()) TypeBuiltinG where
  check (Type _) TyByteStringG = true
  check (Type _) TyIntegerG    = true
  check (Type _) TyStringG     = true
  check _        _             = false


-- |Kind check types.
checkKindG :: KCS n -> Kind () -> TypeG n -> Cool
checkKindG kcs k (TyVarG i)
  = varKindOk
  where
    varKindOk = toCool $ k == kindOf kcs i

checkKindG kcs (Type _) (TyFunG ty1 ty2)
  = ty1KindOk &&& ty2KindOk
  where
    ty1KindOk = checkKindG kcs (Type ()) ty1
    ty2KindOk = checkKindG kcs (Type ()) ty2

checkKindG kcs (Type _) (TyIFixG ty1 k ty2)
  = ty1KindOk &&& ty2KindOk
  where
    ty1Kind   =
      KindArrow () (KindArrow () k (Type ())) (KindArrow () k (Type ()))
    ty1KindOk = checkKindG kcs ty1Kind ty1
    ty2KindOk = checkKindG kcs k ty2

checkKindG kcs (Type _) (TyForallG k body)
  = tyKindOk
  where
    tyKindOk = checkKindG (extKCS k kcs) (Type ()) body

checkKindG _ k (TyBuiltinG tyBuiltin)
  = tyBuiltinKindOk
  where
    tyBuiltinKindOk = check k tyBuiltin

checkKindG kcs (KindArrow () k1 k2) (TyLamG body)
  = bodyKindOk
  where
    bodyKindOk = checkKindG (extKCS k1 kcs) k2 body

checkKindG kcs k' (TyAppG ty1 ty2 k)
  = ty1KindOk &&& ty2KindOk
  where
    ty1Kind   = KindArrow () k k'
    ty1KindOk = checkKindG kcs ty1Kind ty1
    ty2KindOk = checkKindG kcs k ty2

checkKindG _ _ _ = false


instance Check (Kind ()) ClosedTypeG where
  check = checkKindG emptyKCS


instance Check (Kind ()) (Normalized ClosedTypeG) where
  check k ty = check k (unNormalized ty)


-- ** Kind checking state

newtype KCS tyname = KCS{ kindOf :: tyname -> Kind () }

emptyKCS :: KCS Z
emptyKCS = KCS{ kindOf = fromZ }

extKCS :: forall tyname. Kind () -> KCS tyname -> KCS (S tyname)
extKCS k KCS{..} = KCS{ kindOf = kindOf' }
  where
    kindOf' :: S tyname -> Kind ()
    kindOf' FZ     = k
    kindOf' (FS i) = kindOf i


-- ** Type checking

checkTypeG
  :: Eq tyname
  => KCS tyname
  -> TCS tyname name
  -> TypeG tyname
  -> TermG tyname name
  -> Cool
checkTypeG _ tcs ty (VarG i)
  = varTypeOk
  where
    varTypeOk = toCool $ ty == typeOf tcs i

checkTypeG kcs tcs (TyForallG k ty) (TyAbsG tm)
  = tmTypeOk
  where
    tmTypeOk = checkTypeG (extKCS k kcs) (firstTCS FS tcs) ty tm

-- NOTE: In the PlutusCore type checker, the type of the body is
--       not necessarily in normal form. I've opted to force all
--       types to be in normal form, so there's no normalization.
--
checkTypeG kcs tcs (TyFunG ty1 ty2) (LamAbsG tm)
  = tyKindOk &&& tmTypeOk
  where
    tyKindOk = checkKindG kcs (Type ()) ty1
    tmTypeOk = checkTypeG kcs (extTCS ty1 tcs) ty2 tm

checkTypeG kcs tcs ty2 (ApplyG tm1 tm2 (Normalized ty1))
  = tm1TypeOk &&& tm2TypeOk
  where
    tm1TypeOk = checkTypeG kcs tcs (TyFunG ty1 ty2) tm1
    tm2TypeOk = checkTypeG kcs tcs ty1 tm2

checkTypeG kcs tcs vTy (TyInstG tm (Normalized vCod) (Normalized ty) k)
  = tmTypeOk &&& tyKindOk &&& tyOk
  where
    tmTypeOk = checkTypeG kcs tcs (TyForallG k vCod) tm
    tyKindOk = checkKindG kcs k ty
    tyOk = vTy == normalizeTypeG (TyAppG (TyLamG vCod) ty k)

checkTypeG _ _ _ _ = false


instance Check ClosedTypeG ClosedTermG where
  check = checkTypeG emptyKCS emptyTCS

instance Check (Kind (), ClosedTypeG) ClosedTermG where
  check (_k, tyG) = check tyG


-- ** Type checking state

newtype TCS tyname name = TCS{ typeOf :: name -> TypeG tyname }

emptyTCS :: TCS tyname Z
emptyTCS = TCS{ typeOf = fromZ }

extTCS :: forall tyname name. TypeG tyname -> TCS tyname name -> TCS tyname (S name)
extTCS ty TCS{..} = TCS{ typeOf = typeOf' }
  where
    typeOf' :: S name -> TypeG tyname
    typeOf' FZ     = ty
    typeOf' (FS i) = typeOf i

firstTCS :: (tyname -> tyname') -> TCS tyname name -> TCS tyname' name
firstTCS f tcs = TCS{ typeOf = fmap f . typeOf tcs }


-- * Normalisation

-- ** Type reduction

type TySub n m = n -> TypeG m

-- |Extend type substitutions.
extTySub :: TySub n m -> TySub (S n) (S m)
extTySub _ FZ     = TyVarG FZ
extTySub s (FS i) = FS <$> s i


-- |Simultaneous substitution of type variables.
applyTySub :: (n -> TypeG m) -> TypeG n -> TypeG m
applyTySub s (TyVarG i)             = s i
applyTySub s (TyFunG ty1 ty2)       = TyFunG (applyTySub s ty1) (applyTySub s ty2)
applyTySub s (TyIFixG ty1 k ty2)    = TyIFixG (applyTySub s ty1) k (applyTySub s ty2)
applyTySub s (TyForallG k ty)       = TyForallG k (applyTySub (extTySub s) ty)
applyTySub _ (TyBuiltinG tyBuiltin) = TyBuiltinG tyBuiltin
applyTySub s (TyLamG ty)            = TyLamG (applyTySub (extTySub s) ty)
applyTySub s (TyAppG ty1 ty2 k)     = TyAppG (applyTySub s ty1) (applyTySub s ty2) k


-- |Reduce a generated type by a single step, or fail.
stepTypeG :: TypeG n -> Maybe (TypeG n)
stepTypeG (TyVarG _)                  = empty
stepTypeG (TyFunG ty1 ty2)            = (TyFunG <$> stepTypeG ty1 <*> pure ty2)
                                    <|> (TyFunG <$> pure ty1 <*> stepTypeG ty2)
stepTypeG (TyIFixG ty1 k ty2)         = (TyIFixG <$> stepTypeG ty1 <*> pure k <*> pure ty2)
                                    <|> (TyIFixG <$> pure ty1 <*> pure k <*> stepTypeG ty2)
stepTypeG (TyForallG k ty)            = TyForallG <$> pure k <*> stepTypeG ty
stepTypeG (TyBuiltinG _)              = empty
stepTypeG (TyLamG ty)                 = TyLamG <$> stepTypeG ty
stepTypeG (TyAppG (TyLamG ty1) ty2 _) = pure (applyTySub (\case FZ -> ty2; FS i -> TyVarG i) ty1)
stepTypeG (TyAppG ty1 ty2 k)          = (TyAppG <$> stepTypeG ty1 <*> pure ty2 <*> pure k)
                                    <|> (TyAppG <$> pure ty1 <*> stepTypeG ty2 <*> pure k)

-- |Normalise a generated type.
normalizeTypeG :: TypeG n -> TypeG n
normalizeTypeG ty = maybe ty normalizeTypeG (stepTypeG ty)


-- * Errors

-- NOTE: The errors we need to handle in property-based testing are
--       when the generator generates garbage (which shouldn't happen).

data GenError
  = forall tyname. Show tyname => BadTypeG (Kind ()) (TypeG tyname)
  | forall tyname name. (Show tyname, Show name) => BadTermG (TypeG tyname) (TermG tyname name)

instance Show GenError where
  show (BadTypeG k ty) =
    printf "Test generation error: convert type %s at kind %s" (show ty) (show k)
  show (BadTermG ty tm) =
    printf "Test generation error: convert term %s at type %s" (show tm) (show ty)
