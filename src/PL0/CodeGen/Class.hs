{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module PL0.CodeGen.Class where

import           PL0.AST
import           PL0.AST.Class
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

type MonadCode s m c = (
  Code c
  , HasCode s
  , HasProcedures (s c)
  , HasScope (s c)
  , MonadError String m
  , MonadState (s c) m
  )

class Monoid c => Code c where
  data Program c
  generate :: MonadCode s m c => Tree TypedExp -> m (Program c)
  genBlock :: MonadCode s m c => Block TypedExp -> m ()
  genStatement :: MonadCode s m c => Statement TypedExp -> m c
  genDeclarations :: MonadCode s m c => [Declaration TypedExp] -> m ()
  genArgs :: MonadCode s m c => [Declaration TypedExp] -> m ()
  genProcedures :: MonadCode s m c => [Declaration TypedExp] -> m ()
  genExpression :: MonadCode s m c => TypedExp -> m c
  genOp :: OperationName -> c
  genAlloc :: Int -> c

data CodeGenState a where
  CodeGenState :: Code c => Scope -> [Declaration TypedExp] -> c -> CodeGenState c

initialState :: Code c => CodeGenState c
initialState = CodeGenState topLevelScope [] mempty

class HasCode (s :: * -> *) where
  mainCode :: Code c => Lens' (s c) c

instance HasCode CodeGenState where
  mainCode = lens getCode setCode
    where
      getCode (CodeGenState _ _ c) = c
      setCode (CodeGenState sc ps _) c' = CodeGenState sc ps c'

instance Code c => HasScope (CodeGenState c) where
  scope = lens getScope setScope
    where
      getScope (CodeGenState sc _ _) = sc
      setScope (CodeGenState _ ps c) sc' = CodeGenState sc' ps c

class HasProcedures s where
  procedures :: Lens' s [Declaration TypedExp]

instance Code c => HasProcedures (CodeGenState c) where
  procedures = lens getProcs setProcs
    where
      getProcs (CodeGenState _ ps _) = ps
      setProcs (CodeGenState sc _ mc) ps' = CodeGenState sc ps' mc
