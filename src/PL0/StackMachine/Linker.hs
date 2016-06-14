{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module PL0.StackMachine.Linker (link) where

import PL0.AST
import PL0.AST.Class
import PL0.SymbolTable
import PL0.SymbolTable.Scope

import Control.Lens
import Control.Monad.Except
import Control.Monad.State

type MonadLinker s m = (HasScope s, MonadError String m, MonadState s m)

link :: MonadLinker s m => Tree TypedExp -> m (Tree TypedExp)
link t@(Tree (Block decs _)) = do
  linkDecs 2 decs
  return t

linkBlock :: MonadLinker s m => Block TypedExp -> m ()
linkBlock b@(Block decs _) = linkDecs 3 decs

linkDecs :: MonadLinker s m => Int -> [Declaration TypedExp] -> m ()
linkDecs off = linkDecs' 0
  where
    linkDecs' n [] = return ()
    linkDecs' n (ConstDef name e:ds) = do
      entry <- findEntry name <$> use scope
      case entry of
        Just (ConstEntry ty (Left expr)) -> scope %= addEntry name (ConstEntry ty (Right $ n + off))
        Just (ConstEntry ty _) -> error $ "linker: " ++ name ++ " (const) already has an address"
        Just _ -> error $ "linker: " ++ name ++ " not a constant"
        Nothing -> error $ "linker: " ++ name ++ " not found"
      linkDecs' (n + 1) ds
    linkDecs' n (VarDecl name _:ds) = do
      entry <- findEntry name <$> use scope
      case entry of
        Just (VarEntry ty Nothing) -> scope %= addEntry name (VarEntry ty (Just $ n + off))
        Just (VarEntry ty _) -> error $ "linker: " ++ name ++ " (var) already has an address"
        Just _ -> error $ "linker: " ++ name ++ " not a variable"
        Nothing -> error $ "linker: " ++ name ++ " not found"
      linkDecs' (n + 1) ds
    linkDecs' n (ProcedureDef name params block:ds) = do
      scope %= enterScope name
      linkArgs params
      linkBlock block
      scope %= leaveScope
      linkDecs' n ds
    linkDecs' n (d:ds) = linkDecs' n ds

linkArgs :: MonadLinker s m => [Declaration TypedExp] -> m ()
linkArgs = linkArgs' (-1)
  where
    linkArgs' n [] = return ()
    linkArgs' n (VarDecl name ty:ds) = do
      scope %= addEntry name (VarEntry ty (Just n))
      linkArgs' (n - 1) ds
    linkArgs' n (d:ds) = error "linker: something other than VarDecl was in procedure parameters"
