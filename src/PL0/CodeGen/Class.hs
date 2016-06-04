{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PL0.CodeGen.Class where

import           PL0.AST
import           PL0.AST.Class
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

type MonadCode s m c = (Code c, HasCode s, HasScope (s c), MonadError String m, MonadState (s c) m)

class Monoid c => Code c where
  generate :: MonadCode s m c => Tree TypedExp -> m c
  genBlock :: MonadCode s m c => Block TypedExp -> m c
  genStatement :: MonadCode s m c => Statement TypedExp -> m c
  genDeclarations :: MonadCode s m c => [Declaration TypedExp] -> m c
  genExpression :: MonadCode s m c => TypedExp -> m c
  genOp :: OperationName -> c
  genAlloc :: Int -> c

data CodeGenState a where
  CodeGenState :: Code c => Scope -> c -> CodeGenState c

initialState :: Code c => CodeGenState c
initialState = CodeGenState topLevelScope mempty

mCode :: Code c => Lens' (CodeGenState c) c
mCode = lens getCode setCode
  where
    getCode (CodeGenState _ c) = c
    setCode (CodeGenState s c) = CodeGenState s

class HasCode (s :: * -> *) where
  mainCode :: Code c => Lens' (s c) c

instance HasCode CodeGenState where
  mainCode = mCode

pScope :: Code c => Lens' (CodeGenState c) Scope
pScope = lens getScope setScope
  where
    getScope (CodeGenState s _) = s
    setScope (CodeGenState _ c) s' = CodeGenState s' c

instance Code c => HasScope (CodeGenState c) where
  scope = pScope
