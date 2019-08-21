{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Playground.TH
    ( mkFunction
    , mkFunctions
    , mkIotsDefinitions
    , ensureIotsDefinitions
    , mkSingleFunction
    , mkKnownCurrencies
    ) where

import           Data.Text             (pack)
import           IOTS                  (HList (HCons, HNil), Tagged (Tagged))
import qualified IOTS
import           Language.Haskell.TH   (Body (NormalB), Clause (Clause), Dec (FunD, SigD, ValD),
                                        Exp (ListE, LitE, VarE), ExpQ, Info (VarI), Lit (StringL), Name, Pat (VarP), Q,
                                        Type (AppT, ArrowT, ConT, ForallT, ListT, TupleT, VarT), appTypeE, conE, conT,
                                        litT, lookupValueName, mkName, nameBase, reify, strTyLit, varE)
import           Playground.API        (Fn (Fn), FunctionSchema (FunctionSchema), adaCurrency)
import           Schema                (toSchema)
import           Wallet.Emulator.Types (MockWallet)

mkFunctions :: [Name] -> Q [Dec]
mkFunctions names = do
    fns <- traverse mkFunction' names
    let newNames = fmap mkNewName names
        schemas = ValD (VarP (mkName "schemas")) (NormalB (ListE newNames)) []
    pure $ fns <> [schemas]
  where
    mkNewName name = VarE . mkName $ nameBase name ++ "Schema"

iotsBindingName :: String
iotsBindingName = "iotsDefinitions"

mkIotsDefinitions :: [Name] -> Q [Dec]
mkIotsDefinitions names = do
    let applyMonadType :: ExpQ -> ExpQ
        applyMonadType expr = appTypeE expr (conT ''MockWallet)
    iotsDefinition <- [|IOTS.export $(mkTaggedList applyMonadType names)|]
    pure [ValD (VarP (mkName iotsBindingName)) (NormalB iotsDefinition) []]

ensureIotsDefinitions :: Q [Dec]
ensureIotsDefinitions = do
    bound <- lookupValueName iotsBindingName
    case bound of
        Just _ -> pure []
        Nothing ->
            pure
                [ ValD
                      (VarP (mkName iotsBindingName))
                      (NormalB (LitE (StringL "")))
                      []
                ]

mkTaggedList :: (ExpQ -> ExpQ) -> [Name] -> Q Exp
mkTaggedList _ [] = [|HNil|]
mkTaggedList f (x:xs) =
    let nameTag = appTypeE (conE 'Tagged) (litT (strTyLit (nameBase x)))
     in [|HCons ($nameTag $(f (varE x))) $(mkTaggedList f xs)|]

{-# ANN mkFunction ("HLint: ignore" :: String) #-}

mkFunction :: Name -> Q [Dec]
mkFunction _ =
    error $
    "" </> "mkFunction has been replaced by mkFunctions" </> " " </>
    "replace all calls to mkFunction with a single call to mkFunctions, e.g." </>
    " " </>
    " | $(mkFunction 'functionOne)" </>
    " | $(mkFunction 'functionTwo)" </>
    " " </>
    "becomes:" </>
    " " </>
    " | $(mkFunctions ['functionOne, 'functionTwo])" </>
    " "
  where
    a </> b = a <> "\n" <> b

mkSingleFunction :: Name -> Q [Dec]
mkSingleFunction name = do
    dec <- mkFunction' name
    pure [dec]

mkFunction' :: Name -> Q Dec
mkFunction' name = do
    let newName = mkName $ nameBase name ++ "Schema"
        fn = Fn . pack $ nameBase name
    expression <- mkFunctionExp name fn
    pure $ FunD newName [Clause [] (NormalB expression) []]

{-# ANN mkFunctionExp ("HLint: ignore" :: String) #-}

mkFunctionExp :: Name -> Fn -> Q Exp
mkFunctionExp name fn = do
    r <- reify name
    case r of
        (VarI _ as _) ->
            let ts = args as
             in toSchemas fn ts
        _ -> error "Incorrect Name type provided to mkFunction"

{-# ANN toSchemas ("HLint: ignore Redundant bracket" :: String) #-}

toSchemas :: Fn -> [Type] -> Q Exp
toSchemas fn ts = do
    es <- foldr (\t e -> [|toSchema @($(pure t)) : $e|]) [|[]|] ts
    [|FunctionSchema fn $(pure es)|]

{-# ANN args ("HLint: ignore" :: String) #-}

-- Some examples of function types in TH

-- a good function
-- [KindedTV m_6989586621679226019 (AppT (AppT ArrowT StarT) StarT)]
-- [AppT (ConT GHC.Base.Monad) (VarT m_6989586621679226019),AppT (ConT Wallet.API.WalletAPI) (VarT m_6989586621679226019)]
-- (AppT (AppT ArrowT (ConT Ledger.Value.Value))
--       (AppT (AppT ArrowT (ConT Wallet.Emulator.Types.Wallet)) 
--             (AppT (VarT m_6989586621679226019) (TupleT 0))))

-- f :: Maybe Int
-- AppT (ConT GHC.Maybe.Maybe) (ConT GHC.Types.Int)

-- f :: Int
-- ConT GHC.Types.Int

-- f :: [Int]
-- AppT ListT (ConT GHC.Types.Int)

-- FIXME: I think this is the place where we need to check more things about the type of functions
--        we are trying to make schemas for:
-- * They should return (Monad m, WalletAPI m) => m ()
-- * They can only take arguments that have a ToSchema instance (already checked I guess)
-- * I don't think we should ignore things that don't fit, we should error instead
args :: Type -> [Type]
args (AppT (AppT ArrowT t1) as) = t1 : args as -- we take the type an arrow points to without checking what type of type it is
args (AppT (ConT _) _)          = [] -- we ignore type constructors but this doesn't affect us because of the above rule
args (ForallT _ _ as)           = args as -- we expect but ignore constraints but we don't dismiss what they constrain
args (ConT _)                   = [] -- We ignore constant values, I think this allows f :: m () because it ignores and schema is empty
args (TupleT _)                 = [] -- Tuples are ignored, this is how we manage to ignore the return type since it is usually `m ()`
args (AppT (VarT _) t)          = args t -- we ignore type variables but we don't ignore their argument if they have one: m Int becomes Int but then Int is ignored anyway
args a                          = error $ "incorrect type in template haskell function: " ++ show a

-- TODO: add a type declaration to registeredKnownCurrencies
mkKnownCurrencies :: [Name] -> Q [Dec]
mkKnownCurrencies ks = do
    let name = mkName "registeredKnownCurrencies"
        names = fmap VarE ('Playground.API.adaCurrency : ks)
        body = NormalB (ListE names)
        val = ValD (VarP name) body []
        typeName = mkName "KnownCurrency"
        sig = SigD name (AppT ListT (ConT typeName))
    pure [sig, val]
