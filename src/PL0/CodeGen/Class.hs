{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module PL0.CodeGen.Class where

import           PL0.AST
import           PL0.AST.Class
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

class HasScope s where
  scope :: Lens' s Scope

instance HasScope Scope where
  scope = id

type MonadCode s m = (HasScope s, MonadError String m, MonadState s m)

class Monoid c => Code c where
  generate :: MonadCode s m => Tree TypedExp -> m c
  genBlock :: MonadCode s m => Block TypedExp -> m c
  genStatement :: MonadCode s m => Statement TypedExp -> m c
  genDeclarations :: MonadCode s m => [Declaration TypedExp] -> m c
  genExpression :: MonadCode s m => TypedExp -> m c
  genOp :: OperationName -> c
  genAlloc :: Int -> c
