{-# LANGUAGE OverloadedStrings   #-}

module Language.Marlowe.Parser
    ( parseMarlowe
    )
where

import           Data.Void
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Char
import qualified Data.List.NonEmpty            as NonEmpty
import           Text.Megaparsec
import qualified Text.Megaparsec.Expr   as Ex
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.State           as State
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.Marlowe.Compiler
                                         hiding ( commit
                                                , redeem
                                                )
import           Wallet.API                     ( PubKey(..) )

-- data Let = PubKey Integer

data Env = Env {
    counter :: Int,
    parties  :: Map Text PubKey,
    ccIdents :: Map Text IdentCC,
    choices  :: Map Text (IdentChoice, PubKey)
}

emptyEnv = Env 0 Map.empty Map.empty Map.empty

type Parser = ParsecT Void Text (State.State Env)

keywords =
    [ "contract"
    , "oracle"
    , "block"
    , "now"
    , "when"
    , "chose"
    , "before"
    , "timeout"
    , "as"
    , "is"
    , "if"
    , "then"
    , "else"
    , "true"
    , "false"
    , "and"
    , "not"
    , "or"
    , "commits"
    , "pays"
    , "redeem"
    , "end"
    ]

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void space1) lineComment blockComment
  where
    lineComment  = (string "--" <|> string "#") *> void (takeWhileP (Just "character") (/= '\n'))
    blockComment = L.skipBlockComment "{-" "-}"

identChar :: Parser Char
identChar = alphaNumChar

opChar :: Parser Char
opChar = oneOf ("+-*/?!<=>;" :: String)

lexeme = L.lexeme sc

symbol = L.symbol sc

hexInteger = lexeme $ char '0' *> char' 'x' *> L.hexadecimal

integer = lexeme L.decimal

parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy identChar *> sc

reservedOp :: Text -> Parser ()
reservedOp w = string w *> notFollowedBy opChar *> sc

identifier :: Parser Text
identifier = lexeme $ try $ do
    ident <- identifierOrReserved
    when (ident `elem` keywords)
        $  unexpected
        .  Label
        .  NonEmpty.fromList
        $  "reserved "
        ++ (T.unpack ident)
    return ident

idrest = takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '$')

identifierOrReserved = lexeme $ try $ do
    c <- satisfy (\ch -> isAlpha ch || ch == '_' || ch == '$')
    T.cons c <$> idrest

lets = do
    name <- identifier
    reservedOp "="
    p <- person
    modify (\s -> s { parties = Map.insert name p (parties s) })
    return ()

person = (PubKey <$> hexInteger)
    <|> do
        ident <- identifier
        ps <- gets parties
        return $ ps Map.! ident


fresh :: Parser Int
fresh = do
    s <- gets counter
    modify (\s -> s { counter = counter s + 1 })
    return s

block =
    do
            reserved "block"
            integer
        <?> "block expression"

currency = do
    v <- integer
    reserved "Ada"
    return v

valueMul = do
    reservedOp "*"
    return MulValue

valueDiv = do
    reservedOp "/"
    divisor <- valueExpr
    reservedOp "?"
    return (\lhs rhs -> DivValue lhs divisor rhs)

valuePlus = do
    reservedOp "+"
    return AddValue


valueOps = [
          [Ex.InfixL valueMul, Ex.InfixL $ valueDiv ],
          [Ex.InfixL valuePlus]
         ]

value = Value <$> currency <|> parens valueExpr
valueExpr =  Ex.makeExprParser value valueOps

trueObs = do
    reserved "true"
    return TrueObs

falseObs = do
    reserved "false"
    return FalseObs

timeoutObs = do
    reserved "now"
    reservedOp "<"
    t <- block
    return $ BelowTimeout t

choseObs = do
    p <- person
    reserved "chose"
    id <- fresh
    let ident = IdentChoice id
    r <- optional integer
    option ident $ do
        reserved "as"
        name <- identifier
        modify (\s -> s { choices = Map.insert name (ident, p) (choices s) })
        return ident
    case r of
        Just choice -> return $ PersonChoseThis ident p choice
        Nothing -> return $ PersonChoseSomething ident p

choiseIsObs = do
    name <- identifier
    reserved "is"
    choice <- integer
    cs <- gets choices
    let (ident, person) = cs Map.! name
    return $ PersonChoseThis ident person choice

cmpObs = do
    lhs <- valueExpr
    reservedOp ">="
    rhs <- valueExpr
    return $ ValueGE lhs rhs

notObs = do
    reserved "not"
    return NotObs

andObs = do
    reserved "and"
    return AndObs

orObs = do
    reserved "or"
    return OrObs

observationOps = [
    [Ex.Prefix notObs ],
    [Ex.InfixL andObs ],
    [Ex.InfixL orObs]
   ]

observation = trueObs
    <|> falseObs
    <|> timeoutObs
    <|> try choiseIsObs
    <|> choseObs
    <|> try cmpObs
    <|> parens observationExpr

observationExpr = Ex.makeExprParser observation observationOps


elseBlock = do
    reserved "else"
    body

commit = do
    pk <- person
    reserved "commits"
    v <- value
    reserved "before"
    d <- block
    reserved "timeout"
    t  <- block
    id <- fresh
    id <- option (IdentCC id) $ do
        reserved "as"
        name <- identifier
        modify (\s -> s { ccIdents = Map.insert name (IdentCC id) (ccIdents s) })
        return $ (IdentCC id)
    cont  <- body
    other <- option Null $ elseBlock
    return $ CommitCash id
                        pk
                        v
                        (fromIntegral d)
                        (fromIntegral t)
                        cont
                        other

redeem = do
    reserved "redeem"
    ident <- identifier
    ccs   <- gets ccIdents
    cont  <- option Null $ elseBlock
    case Map.lookup ident ccs of
        Just id -> return $ RedeemCC id cont
        Nothing -> error "Sigh"

pay = do
    from <- person
    reserved "pays"
    to <- person
    v  <- valueExpr
    reserved "before"
    t    <- block
    cont <- body
    return $ Pay (IdentPay 1)
                 from
                 to
                 v
                 (fromIntegral t)
                 cont

whenContract = do
    reserved "when"
    obs <- observationExpr
    reserved "before"
    t    <- block
    reserved "then"
    cont <- body
    cont2 <- option Null elseBlock
    return $ When obs t cont cont2

choiceContract = do
    reserved "if"
    obs <- observationExpr
    reserved "then"
    cont <- body
    cont2 <- option Null elseBlock
    return $ Choice obs cont cont2


nullContract = do
    reserved "end"
    return Null

emptyContract = do
    string ""
    return Null

contract = redeem
    <|> whenContract
    <|> choiceContract
    <|> try commit
    <|> pay
    <|> nullContract
    <|> emptyContract
    <?> "contract constructor"

body = braces cont <|> cont <?> "contract"
  where
    cont = do
        many $ try lets
        contract

contractDef = do
    reserved "contract"
    identifier
    braces body

parser = many contractDef

contents p = between sc eof p


parseMarlowe :: Text -> Either (ParseError Char Void) [Contract]
parseMarlowe string = evalState (runParserT (contents parser) "<stdin>" string) emptyEnv

