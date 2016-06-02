{
module PL0.Lexer (
  AlexPosn(..)
  , Token(..)
  , TokenType(..)
  , scanTokens
) where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  "//".* ;
  ":=" { \pos s -> Token pos ASSIGN }
  ":"  { \pos s -> Token pos COLON }
  ";"  { \pos s -> Token pos SEMICOLON }
  ".." { \pos s -> Token pos RANGE }
  "," { \pos s -> Token pos COMMA }
  "(" { \pos s -> Token pos LPAREN }
  ")"  { \pos s -> Token pos RPAREN }
  "["  { \pos s -> Token pos LBRACKET }
  "]"  { \pos s -> Token pos RBRACKET }
  "="  { \pos s -> Token pos EQUALS }
  "!=" { \pos s -> Token pos NEQUALS }
  ">=" { \pos s -> Token pos GEQUALS }
  "<=" { \pos s -> Token pos LEQUALS }
  ">"  { \pos s -> Token pos GREATER }
  "<"  { \pos s -> Token pos LESS }
  "+"  { \pos s -> Token pos PLUS }
  "-"  { \pos s -> Token pos MINUS }
  "*"  { \pos s -> Token pos TIMES }
  "/"  { \pos s -> Token pos DIVIDE }
  "begin" { \pos s -> Token pos KW_BEGIN }
  "call"  { \pos s -> Token pos KW_CALL }
  "const" { \pos s -> Token pos KW_CONST }
  "do"    { \pos s -> Token pos KW_DO }
  "else"  { \pos s -> Token pos KW_ELSE }
  "end"   { \pos s -> Token pos KW_END }
  "if"    { \pos s -> Token pos KW_IF }
  "procedure"  { \pos s -> Token pos KW_PROC }
  "read"  { \pos s -> Token pos KW_READ }
  "then"  { \pos s -> Token pos KW_THEN }
  "type"  { \pos s -> Token pos KW_TYPE }
  "var"   { \pos s -> Token pos KW_VAR }
  "while" { \pos s -> Token pos KW_WHILE }
  "write" { \pos s -> Token pos KW_WRITE }
  "false" { \pos s -> Token pos KW_FALSE }
  "true" { \pos s -> Token pos KW_TRUE }
  $digit+ { \pos s -> Token pos (NUMBER $ read s) }
  $alpha [$alpha $digit]* { \pos s -> Token pos (IDENTIFIER s) }

{

data Token = Token AlexPosn TokenType

data TokenType = ASSIGN
               | COLON
               | SEMICOLON
               | RANGE
               | COMMA
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
               | KW_FALSE
               | KW_TRUE
               | NUMBER Int
               | IDENTIFIER String
               | EOF
               deriving (Eq,Show)

scanTokens :: String -> Either String [Token]
scanTokens str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_bs,str) = case alexScan inp 0 of
      AlexEOF -> Right [Token pos EOF]
      AlexError ((AlexPn _ line col),c,_,_) -> Left $ concat [
        "lexical error at line "
        , show line
        , ", column "
        , show (col - 1)
        , ": unexpected '"
        , pure c
        , "'"
        ]
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> fmap ((act pos $ take len str) :) (go inp')

}
