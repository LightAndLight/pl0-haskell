{
module PL0.Lexer (Token(..), scanTokens) where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  "//".* ;
  ":=" { \s -> ASSIGN }
  ":"  { \s -> COLON }
  ";"  { \s -> SEMICOLON }
  ".." { \s -> RANGE }
  "," { \s -> COMMA }
  "(" { \s -> LPAREN }
  ")"  { \s -> RPAREN }
  "["  { \s -> LBRACKET }
  "]"  { \s -> RBRACKET }
  "="  { \s -> EQUALS }
  "!=" { \s -> NEQUALS }
  ">=" { \s -> GEQUALS }
  "<=" { \s -> LEQUALS }
  ">"  { \s -> GREATER }
  "<"  { \s -> LESS }
  "+"  { \s -> PLUS }
  "-"  { \s -> MINUS }
  "*"  { \s -> TIMES }
  "/"  { \s -> DIVIDE }
  "begin" { \s -> KW_BEGIN }
  "call"  { \s -> KW_CALL }
  "const" { \s -> KW_CONST }
  "do"    { \s -> KW_DO }
  "else"  { \s -> KW_ELSE }
  "end"   { \s -> KW_END }
  "if"    { \s -> KW_IF }
  "procedure"  { \s -> KW_PROC }
  "read"  { \s -> KW_READ }
  "then"  { \s -> KW_THEN }
  "type"  { \s -> KW_TYPE }
  "var"   { \s -> KW_VAR }
  "while" { \s -> KW_WHILE }
  "write" { \s -> KW_WRITE }
  $digit+ { NUMBER . read }
  $alpha [$alpha $digit]* { IDENTIFIER }

{

data Token = ASSIGN
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
           | NUMBER Int
           | IDENTIFIER String
           | EOF
           deriving (Eq,Show)

scanTokens :: String -> Either String [Token]
scanTokens str = go ('\n',[],str)
  where
    go inp@(_,_bs,str) = case alexScan inp 0 of
      AlexEOF -> Right [EOF]
      AlexError _ -> Left "lexical error"
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> fmap (act (take len str) :) (go inp')

}
