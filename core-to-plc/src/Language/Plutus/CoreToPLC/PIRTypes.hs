module Language.Plutus.CoreToPLC.PIRTypes where

import qualified Language.PlutusCore        as PLC
import qualified Language.PlutusCore.MkPlc  as PLC

import qualified Language.PlutusIR          as PIR
import qualified Language.PlutusIR.Compiler as PIR

type PLCKind = PLC.Kind ()
type PLCType = PLC.Type PLC.TyName ()
type PLCTerm = PLC.Term PLC.TyName PLC.Name ()

type PLCVar = PLC.VarDecl PLC.TyName PLC.Name ()
type PLCTyVar = PLC.TyVarDecl PLC.TyName ()

type PIRKind = PIR.Kind ()
type PIRType = PIR.Type PIR.TyName ()
type PIRTerm = PIR.Term PIR.TyName PIR.Name ()



type PIRVar = PIR.VarDecl PIR.TyName PIR.Name ()
type PIRTyVar = PIR.TyVarDecl PIR.TyName ()

unsafeCompileTerm :: PIR.Term PIR.TyName PIR.Name () -> PLC.Term PLC.TyName PLC.Name ()
unsafeCompileTerm pirterm = case (PIR.runCompiling $ PIR.compileTerm pirterm) of
                                Left t -> error "impossible"
                                Right t -> t
