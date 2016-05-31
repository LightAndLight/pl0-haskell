{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module PL0.CodeGen.Class where

import           PL0.AST
import           PL0.AST.Class
import           PL0.SymbolTable.Scope

import           Control.Monad.Except
import           Control.Monad.State

type MonadCode m = (MonadError String m, MonadState Scope m)

class Monoid c => Code c where
  generate :: MonadCode m => Tree TypedExp -> m c
  genBlock :: MonadCode m => Block TypedExp -> m c
  genStatement :: MonadCode m => Statement TypedExp -> m c
  genDeclarations :: MonadCode m => [Declaration TypedExp] -> m c
  genExpression :: MonadCode m => TypedExp -> m c
  genOp :: OperationName -> c
  genAlloc :: Int -> c
