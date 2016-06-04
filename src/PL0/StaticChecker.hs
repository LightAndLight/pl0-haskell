{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module PL0.StaticChecker (checkProgram) where

import           PL0.AST
import           PL0.AST.Class
import           PL0.SymbolTable
import           PL0.SymbolTable.Scope

import           Control.Applicative   ((<|>))
import Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.DList            (DList, singleton)
import           Data.ITree.Zipper
import qualified Data.Map              as M

type MonadChecker s m = (HasScope s, MonadError String m, MonadState s m)

checkProgram :: (Expression e, MonadChecker s m) => Tree e -> m (Tree TypedExp)
checkProgram (Tree block) = Tree <$> checkBlock block

checkBlock :: (Expression e, MonadChecker s m) => Block e -> m (Block TypedExp)
checkBlock (Block decs statement) = Block
  <$> traverse checkDeclaration decs
  <*> checkStatement statement

checkDeclaration :: (Expression e, MonadChecker s m) => Declaration e -> m (Declaration TypedExp)
checkDeclaration c@(ConstDef name expr)= do
  table <- getTable <$> use scope
  if M.member name table
    then throwError $ name ++ " already defined"
    else do
      checked <- checkExpression expr
      scope %= addEntry name (ConstEntry (getType checked) (Left checked))
      return $ ConstDef name checked

checkDeclaration t@(TypeDef name ty) = do
  table <- getTable <$> use scope
  if M.member name table
    then throwError $ name ++ " already defined"
    else do
      resolvedTy <- resolve ty
      scope %= addEntry name (TypeEntry resolvedTy)
      return $ TypeDef name resolvedTy

checkDeclaration v@(VarDecl name ty) = do
  table <- getTable <$> use scope
  if M.member name table
    then throwError $ name ++ " already declared"
    else do
      resolvedTy <- resolve ty
      scope %= addEntry name (VarEntry (TRef resolvedTy) Nothing)
      return $ VarDecl name resolvedTy

checkDeclaration p@(ProcedureDef name args block) = do
  table <- getTable <$> use scope
  if M.member name table
    then throwError $ "procedure " ++ name ++ " already defined"
    else do
      scope %= extendScope name
      checkedArgs <- traverse checkDeclaration args
      checkedBlock <- checkBlock block
      scope %= leaveScope
      argTypes <- traverse (\(VarDecl _ ty) -> resolve ty) checkedArgs
      scope %= addEntry name (ProcEntry argTypes Nothing)
      return $ ProcedureDef name checkedArgs checkedBlock

checkStatement :: (Expression e, MonadChecker s m) => Statement e -> m (Statement TypedExp)
checkStatement (Assignment lvalue expr) = do
  checkedLValue <- checkExpression lvalue
  case checkedLValue of
    Variable (TRef ty) _ -> do
      checkedExpression <- dereference <$> checkExpression expr >>= coerceExpression ty
      return $ Assignment checkedLValue checkedExpression
    Variable _ _ -> error "error: encountered variable of non-ref type"
    _ -> throwError "can't assign to non-variable"
checkStatement (Read lvalue) = do
  checkedLValue <- checkExpression lvalue
  case checkedLValue of
    Variable (TRef ty) _ -> do
      coerceTo ty TInt
      return $ Read checkedLValue
    Variable _ _ -> error "error: encountered variable of non-ref type"
    _ -> throwError "can't read to non-variable"
checkStatement (Write expr) = do
  expr' <- dereference <$> checkExpression expr >>= coerceExpression TInt
  return $ Write expr'
checkStatement (While cond statement) = do
  cond' <- dereference <$> checkExpression cond >>= coerceExpression TBool
  statement' <- checkStatement statement
  return $ While cond' statement'
checkStatement (If cond thenStatement elseStatement) = do
  cond' <- dereference <$> checkExpression cond >>= coerceExpression TBool
  then' <- checkStatement thenStatement
  else' <- checkStatement elseStatement
  return $ If cond' then' else'
checkStatement c@(CallStatement name args) = do
  scope <- use scope
  case findEntry name scope of
    Just (ProcEntry ty offset) -> do
      args' <- checkArgs ty =<< traverse checkExpression args
      return $ CallStatement name args'
    Just _ -> throwError "expected procedure"
    Nothing -> throwError $ "procedure \"" ++ name ++ "\" not defined"
checkStatement (Compound statements) = Compound <$> traverse checkStatement statements
checkStatement SError = return SError
