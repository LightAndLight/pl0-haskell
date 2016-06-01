{
module PL0.Parser (parseTokens) where

import PL0.AST
import PL0.AST.Class
import PL0.Lexer
import PL0.SymbolTable
}

%name parseTokens
%monad { Either String } { (>>=) } { return }
%tokentype { Token }
%error { parseError }

%token
  ':=' { ASSIGN }
  ':' { COLON }
  ';' { SEMICOLON }
  '..' { RANGE }
  ',' { COMMA }
  '(' { LPAREN }
  ')' { RPAREN }
  '[' { LBRACKET }
  ']' { RBRACKET }
  '=' { EQUALS }
  '!=' { NEQUALS }
  '>=' { GEQUALS }
  '<=' { LEQUALS }
  '<' { LESS }
  '>' { GREATER }
  '+' { PLUS }
  '-' { MINUS }
  '*' { TIMES }
  '/' { DIVIDE }
  begin { KW_BEGIN }
  call { KW_CALL }
  const { KW_CONST }
  do { KW_DO }
  else { KW_ELSE }
  end { KW_END }
  if { KW_IF }
  then { KW_THEN }
  proc { KW_PROC }
  read { KW_READ }
  type { KW_TYPE }
  var { KW_VAR }
  while { KW_WHILE }
  write { KW_WRITE }
  false { KW_FALSE }
  true { KW_TRUE }
  number { NUMBER $$ }
  identifier { IDENTIFIER $$ }
  eof { EOF }

%%

Program : Block eof { Tree $1 }

Block : Declarations CompoundStatement { Block $1 $2 }

Declarations : {- empty -} { [] }
             | Declaration Declarations { $1 ++ $2 }

Declaration : ConstDefList { $1 }
            | TypeDefList { $1 }
            | VarDeclList { $1 }
            | ProcedureDef { $1 }

ConstDefList : const ConstDefs { $2 }

ConstDefs : ConstDef { [$1] }
          | ConstDef ConstDefs { $1 : $2 }

ConstDef : identifier '=' Constant ';' { ConstDef $1 $3 }

Constant : number { UProxy (Const TInt $1) }
         | identifier { Identifier $1 }
         | '-' Constant { UOp negateOp [$2] }

TypeDefList : type TypeDefs { $2 }

TypeDefs : TypeDef { [$1] }
         | TypeDef TypeDefs { $1 : $2 }

TypeDef : identifier '=' Type ';' { TypeDef $1 $3 }

Type : TypeIdentifier { $1 }
     | SubrangeType { $1 }

TypeIdentifier : identifier { TId $1 }

SubrangeType : '[' Constant '..' Constant ']' { UTSub $2 $4 }

VarDeclList : var VarDecls { $2 }

VarDecls : VarDecl { [$1] }
         | VarDecl VarDecls { $1 : $2 }

VarDecl : identifier ':' TypeIdentifier ';' { VarDecl $1 $3 }

ProcedureDef : proc identifier '(' FormalParameters ')' '=' Block ';' { [ProcedureDef $2 $4 $7] }

FormalParameters : {- empty -} { [] }
                 | SomeFormalParameters { $1 }

SomeFormalParameters : FormalParameter { [$1] }
                     | FormalParameter ',' SomeFormalParameters { $1 : $3 }

FormalParameter : identifier ':' TypeIdentifier { VarDecl $1 $3 }

CompoundStatement : begin StatementList end { Compound $2 }

StatementList : Statement { [$1] }
              | Statement ';' StatementList { $1 : $3 }

Statement : Assignment { $1 }
          | CallStatement { $1 }
          | ReadStatement { $1 }
          | WriteStatement { $1 }
          | WhileStatement { $1 }
          | IfStatement { $1 }
          | CompoundStatement { $1 }

Assignment : LValue ':=' Condition { Assignment $1 $3 }

CallStatement : call identifier '(' ActualParameters ')' { CallStatement $2 $4 }

ActualParameters : {- empty -} { [] }
                 | SomeActualParameters { $1 }

SomeActualParameters : Condition { [$1] }
                     | Condition ',' SomeActualParameters { $1 : $3 }

ReadStatement : read LValue { Read $2 }

WriteStatement : write Exp { Write $2 }

WhileStatement : while Condition do Statement { While $2 $4 }

IfStatement : if Condition then Statement else Statement { If $2 $4 $6 }

Condition : Exp { $1 }
          | Exp RelOp Exp { UOp $2 [$1,$3] }

RelOp : '=' { equalsOp }
      | '!=' { nequalsOp }
      | '<=' { lequalsOp }
      | '<' { lessOp }
      | '>=' { gequalsOp }
      | '>' { greaterOp }

Exp : false { UProxy (Const TBool 0) }
    | true { UProxy (Const TBool 1) }
    | LeftTerm { $1 }
    | LeftTerm PlusMinus Term { UOp $2 [$1,$3] }

LeftTerm : Term { $1 }
         | '+' Term { $2 }
         | '-' Term { UOp negateOp [$2] }

PlusMinus : '+' { plusOp }
          | '-' { minusOp }

Term : Factor { $1 }
     | Factor TimesDivide Factor { UOp $2 [$1,$3] }

TimesDivide : '*' { timesOp }
            | '/' { divideOp }

Factor : '(' Condition ')' { $2 }
       | number { UProxy (Const TInt $1) }
       | LValue { $1 }

LValue : identifier { Identifier $1 }

{
parseError tokens = Left "parse error"
}
