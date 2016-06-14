{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module PL0.AST where

import           PL0.AST.Class
import           PL0.SymbolTable
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable

deriving instance Show e => Show (Tree e)
deriving instance Show e => Show (Block e)
deriving instance Show e => Show (Declaration e)
deriving instance Show e => Show (Statement e)
deriving instance Show UnresolvedType
deriving instance Show UndecidedOp
deriving instance Show Exp

data Tree e where
  Tree :: Expression e => Block e -> Tree e

data Block e where
  Block :: (Show e, Expression e) => [Declaration e] -> Statement e -> Block e

data Declaration e where
  ConstDef :: (Show e, Expression e) => String -> e -> Declaration e
  TypeDef :: (Type t, Show t, Expression e) => String -> t -> Declaration e
  VarDecl :: (Type t, Show t, Expression e) => String -> t -> Declaration e
  ProcedureDef :: String -> [Declaration e] -> Block e -> Declaration e

data Statement e where
  Assignment :: (Show e, Expression e) => e -> e -> Statement e
  Read :: (Show e, Expression e) => e -> Statement e
  Write :: (Show e, Expression e) => e -> Statement e
  While :: (Show e, Expression e) => e -> Statement e -> Statement e
  If :: (Show e, Expression e) => e -> Statement e -> Statement e -> Statement e
  CallStatement :: (Show e, Expression e) => String -> [e] -> Statement e
  Compound :: [Statement e] -> Statement e
  SError :: Statement e

data UnresolvedType where
  TId :: String -> UnresolvedType
  UTSub :: (Show from, Show to, Expression from, Expression to) => from -> to -> UnresolvedType

instance Type UnresolvedType where
  resolve (TId name) = do
    scope <- use scope
    case findEntry name scope of
      Just (TypeEntry ty) -> resolve ty
      Just _ -> throwError "expected type"
      Nothing -> throwError $ "type \"" ++ name ++ "\" not defined"
  resolve (UTSub from to) = do
    from' <- checkExpression from
    to' <- checkExpression to
    ty' <- coerceTo (getType from') (getType to')
    return $ TSub ty' from' to'

data UndecidedOp where
  OOperation :: OperationName -> [TFunction] -> UndecidedOp
  Operation :: OperationName -> TFunction -> UndecidedOp

instance Operation UndecidedOp where
  decide (Operation name tf) es = return . DecidedOp name tf $ map dereference es
  decide (OOperation name [tf@(TFunction ts _)]) es = DecidedOp name tf <$> checkArgs ts es
  decide (OOperation name (tf@(TFunction ts _):fs)) es = catchError
    (DecidedOp name tf <$> checkArgs ts es)
    (const $ decide (OOperation name fs) es)

data Exp where
  UOp :: (Show e, Show o, Expression e, Operation o) => o -> [e] -> Exp
  UProxy :: TypedExp -> Exp
  Identifier :: String -> Exp

instance Expression Exp where
  checkExpression (UOp o exprs) = traverse checkExpression exprs >>= decide o
  checkExpression (UProxy e) = return e
  checkExpression (Identifier name) = do
    scope <- use scope
    case findEntry name scope of
      Just (ConstEntry ty (Left expr)) -> do
        ty' <- resolve ty
        expr' <- checkExpression expr
        coerceExpression ty' $ dereference expr'
      Just (ConstEntry _ _) -> error "static check: const entry can't have an address yes"
      Just (VarEntry ty _) -> Variable <$> resolve ty <*> pure name
      Just _ -> throwError "expected constant or variable"
      Nothing -> throwError $ name ++ " not defined"

binary :: ResolvedType -> ResolvedType -> TFunction
binary ty = TFunction [ty,ty]

relational :: TFunction
relational = binary TInt TBool

equalsOp :: UndecidedOp
equalsOp = OOperation Equals [binary TBool TBool,relational]

nequalsOp :: UndecidedOp
nequalsOp = OOperation NEquals [binary TBool TBool,relational]

lequalsOp :: UndecidedOp
lequalsOp = Operation LEquals relational

lessOp :: UndecidedOp
lessOp = Operation Less relational

greaterOp :: UndecidedOp
greaterOp = Operation Greater relational

gequalsOp :: UndecidedOp
gequalsOp = Operation GEquals relational

plusOp :: UndecidedOp
plusOp = Operation Plus $ binary TInt TInt

minusOp :: UndecidedOp
minusOp = Operation Minus $ binary TInt TInt

timesOp :: UndecidedOp
timesOp = Operation Times $ binary TInt TInt

divideOp :: UndecidedOp
divideOp = Operation Divide $ binary TInt TInt

negateOp :: UndecidedOp
negateOp = Operation Negate $ TFunction [TInt] TInt
