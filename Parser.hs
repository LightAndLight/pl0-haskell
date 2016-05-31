{-# LANGUAGE FlexibleContexts #-}

module PL0.Parser (parseProgram) where

import           PL0.AST
import           PL0.SymbolTable

import           Control.Applicative ((<$>),(<*>),liftA2)
import           Control.Monad.State
import           Control.Monad.State.Class
import Data.Char (isDigit, isLetter)
import qualified Data.Map as M
import           Text.Megaparsec
import           qualified Text.Megaparsec.Lexer as L (lexeme, symbol)
import           Text.Megaparsec.Prim
import           Text.Megaparsec.ShowToken
import           Text.Megaparsec.String

data TokenError = UNEXPECTED Char
                | ILLEGAL Char

data Token = ASSIGN
               | COLON
               | SEMICOLON
               | RANGE
               | LPAREN
               | RPAREN
               | LBRACKET
               | RBRACKET
               | EQUALS
               | NEQUALS
               | GEQUALS
               | LEQUALS
               | GREATER
               | LESS
               | PLUS
               | MINUS
               | TIMES
               | DIVIDE
               | KW_BEGIN
               | KW_CALL
               | KW_CONST
               | KW_DO
               | KW_ELSE
               | KW_END
               | KW_IF
               | KW_PROC
               | KW_READ
               | KW_THEN
               | KW_TYPE
               | KW_VAR
               | KW_WHILE
               | KW_WRITE
               | NUMBER String
               | IDENTIFIER String
               | EOF
               deriving Show

numberToken :: String -> Either TokenError Token
numberToken str = NUMBER <$> numberToken' str
  where
    numberToken' c
      | isDigit c = Right c
      | isLetter c = Left $ UNEXPECTED c
      | otherwise = 

tokenize :: String -> Either TokenError [Token]
tokenize = map tokenize' . words
  where
    tokenize' str = case str of
      ""   -> Right EOF
      ":=" -> Right ASSIGN
      ":"  -> Right COLON
      ";"  -> Right SEMICOLON
      ".." -> Right RANGE
      "("  -> Right LPAREN
      ")"  -> Right RPAREN
      "["  -> Right LBRACKET
      "]"  -> Right RBRACKET
      "="  -> Right EQUALS
      "!=" -> Right NEQUALS
      ">=" -> Right GEQUALS
      "<=" -> Right LEQUALS
      ">"  -> Right GREATER
      "<"  -> Right LESS
      "+"  -> Right PLUS
      "-"  -> Right MINUS
      "*"  -> Right TIMES
      "/"  -> Right DIVIDE
      "begin" -> Right KW_BEGIN
      "call"  -> Right KW_CALL
      "const" -> Right KW_CONST
      "do"    -> Right KW_DO
      "else"  -> Right KW_ELSE
      "end"   -> Right KW_END
      "if"    -> Right KW_IF
      "proc"  -> Right KW_PROC
      "read"  -> Right KW_READ
      "then"  -> Right KW_THEN
      "type"  -> Right KW_TYPE
      "var"   -> Right KW_VAR
      "while" -> Right KW_WHILE
      "write" -> Right KW_WRITE
      (c:_)   -> if isDigit c
                    then numberToken str
                    else if isLetter c
                            then identifierToken c
                            else Left $ ILLEGAL c

instance ShowToken Token where
  showToken = show

lexeme :: MonadParsec s m Char => m a -> m a
lexeme = L.lexeme (skipMany spaceChar)

symbol :: MonadParsec s m Char => String -> m String
symbol = L.symbol (skipMany spaceChar)

identifier :: MonadParsec s m Char => m String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

number :: MonadParsec s m Char => m Int
number = read <$> lexeme (liftA2 (:) digitChar (many digitChar))

equals :: MonadParsec t m Char => m String
equals = symbol "="

semicolon :: MonadParsec t m Char => m String
semicolon = symbol ";"

minus :: MonadParsec t m Char => m String
minus = symbol "-"

plus :: MonadParsec t m Char => m String
plus = symbol "+"

multiply :: MonadParsec t m Char => m String
multiply = symbol "*"

divide :: MonadParsec t m Char => m String
divide = symbol "/"

lbracket :: MonadParsec t m Char => m String
lbracket = symbol "["

rbracket :: MonadParsec t m Char => m String
rbracket = symbol "]"

lparen :: MonadParsec t m Char => m String
lparen = symbol "("

rparen :: MonadParsec t m Char => m String
rparen = symbol ")"

range :: MonadParsec t m Char => m String
range = symbol ".."

begin :: MonadParsec t m Char => m String
begin = lexeme $ string "begin"

end :: MonadParsec t m Char => m String
end = lexeme $ string "end"

constKW :: MonadParsec t m Char => m String
constKW = lexeme $ string "const"

typeKW :: MonadParsec t m Char => m String
typeKW = lexeme $ string "type"

var :: MonadParsec t m Char => m String
var = lexeme $ string "var"

proc :: MonadParsec t m Char => m String
proc = lexeme $ string "proc"

greater :: MonadParsec t m Char => m String
greater = symbol ">"

less :: MonadParsec t m Char => m String
less = symbol "<"

equal :: MonadParsec t m Char => m String
equal = symbol "="

assign :: MonadParsec t m Char => m String
assign = symbol ":="

greaterEqual :: MonadParsec t m Char => m String
greaterEqual = symbol ">="

lessEqual :: MonadParsec t m Char => m String
lessEqual = symbol "<="

notEqual :: MonadParsec t m Char => m String
notEqual = symbol "!="

call :: MonadParsec t m Char => m String
call = lexeme $ string "call"

readKW :: MonadParsec t m Char => m String
readKW = lexeme $ string "read"

write :: MonadParsec t m Char => m String
write = lexeme $ string "write"

while :: MonadParsec t m Char => m String
while = lexeme $ string "while"

doKW :: MonadParsec t m Char => m String
doKW = lexeme $ string "do"

ifKW :: MonadParsec t m Char => m String
ifKW = lexeme $ string "if"

thenKW :: MonadParsec t m Char => m String
thenKW = lexeme $ string "then"

elseKW :: MonadParsec t m Char => m String
elseKW = lexeme $ string "else"

program :: (MonadState SymbolTable m, MonadParsec t m Char) => m (Tree Program)
program = Tree <$> getPosition <*>
  (Program <$> block)

block :: (MonadState SymbolTable m, MonadParsec t m Char) => m (Tree Block)
block = Tree <$> getPosition <*>
  (Block <$> many declaration <*> statement)

declaration :: (MonadState SymbolTable m, MonadParsec t m Char) => m (Tree Declaration)
declaration = Tree <$> getPosition <*>
     (try constKW *> (ConstDefList <$> some constDef)
  <|> try typeKW *> (TypeDefList <$> some typeDef)
  <|> try var *> (VarDecList <$> some varDec)
  <|> procedureDef)

constant :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree Exp)
constant = do
  pos <- getPosition
  table <- get
  e <- try (Const tInt <$> number)
   <|> try (lookupVariable table <$> identifier)
   <|> minus *> pure (UnOp tInt Negate) <*> constant
  return $ Tree pos e

constDef :: (MonadState SymbolTable m, MonadParsec t m Char) => m (Tree ConstDef)
constDef = Tree <$> getPosition <*>
  (ConstDef <$> identifier <* equals <*> constant <* semicolon)

subrangeType :: (MonadState SymbolTable m, MonadParsec s m Char) => m Type
subrangeType = do
  lbracket
  from <- constant
  range
  to <- constant
  rbracket
  return $ SubrangeType from to

typeDef :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree TypeDef)
typeDef = do
  pos <- getPosition
  table <- get
  name <- identifier
  equals
  ty <- if M.member name table
    then return . TError $ name ++ " already defined"
    else try (lookupType table <$> identifier) <|> subrangeType
  modify (M.insert name $ TypeEntry pos ty)
  return . Tree pos $ TypeDef name ty

varDec :: MonadParsec t m Char => m (Tree VarDecl)
varDec = error "varDec NIH"

procedureDef :: MonadParsec t m Char => m Declaration
procedureDef = proc *> error "procDef NIH"

statement :: (MonadState SymbolTable m, MonadParsec t m Char) => m (Tree Statement)
statement = Tree <$> getPosition <*>
     (try assignStatement
  <|> try callStatement
  <|> try readStatement
  <|> try writeStatement
  <|> try whileStatement
  <|> try ifStatement
  <|> compoundStatement)

assignStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
assignStatement = do
  table <- get
  var <- lookupVariable table <$> identifier
  assign
  cond <- condition
  return $ Assignment var cond

callStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
callStatement = do
  call
  table <- get
  node <- lookupProcedure table <$> identifier
  lparen
  params <- actualParams
  rparen
  return $ node params

actualParams :: MonadParsec t m Char => m [Tree Exp]
actualParams = return []

readStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
readStatement = do
  readKW
  table <- get
  expr <- lookupVariable table <$> identifier
  return $ Read expr

writeStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
writeStatement = write *> pure Write <*> expression

whileStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
whileStatement = while *> pure While <*> condition <* doKW <*> statement

ifStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
ifStatement = ifKW *> pure If <*> condition <* thenKW <*> statement <* elseKW <*> statement

condition :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree Exp)
condition = do
  pos <- getPosition
  try (fmap (Tree pos) $ BinOp tBool <$> expression <*> relOp <*> expression)
    <|> expression

expression :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree Exp)
expression = do
  pos <- getPosition
  let parseLeft = try (plus *> term) <|> Tree pos <$> (minus *> pure (UnOp tInt Negate) <*> term)
  try (fmap (Tree pos) $ BinOp tInt <$> parseLeft <*> parseOp <*> term)
    <|> parseLeft
  where
    parseOp = try (plus *> pure Plus) <|> minus *> pure Minus

term :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree Exp)
term = do
  pos <- getPosition
  try (fmap (Tree pos) $ BinOp tInt <$> factor <*> parseOp <*> factor)
    <|> factor
  where
    parseOp = try (multiply *> pure Multiply) <|> divide *> pure Divide

factor :: (MonadState SymbolTable m, MonadParsec s m Char) => m (Tree Exp)
factor = do
  pos <- getPosition
  table <- get
  try (lparen *> condition <* rparen)
    <|> try (Tree pos . Const tInt <$> number)
    <|> Tree pos . lookupVariable table <$> identifier

relOp :: MonadParsec s m Char => m BinOp
relOp = try (greaterEqual *> pure GEquals)
    <|> try (lessEqual *> pure LEquals)
    <|> try (greater *> pure Greater)
    <|> try (less *> pure Less)
    <|> try (notEqual *> pure NEquals)
    <|> equals *> pure Equals

compoundStatement :: (MonadState SymbolTable m, MonadParsec t m Char) => m Statement
compoundStatement = begin *> (Compound <$> statementList) <* end

statementList :: (MonadState SymbolTable m, MonadParsec t m Char) => m [Tree Statement]
statementList = liftA2 (:) statement . many $ semicolon *> statement

parseProgram :: String -> String -> Either ParseError (Tree Program)
parseProgram name text = evalState (runParserT program name text) newSymbolTable
