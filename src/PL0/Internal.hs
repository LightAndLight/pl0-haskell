{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving    #-}

module PL0.Internal where

import Control.Monad.Except
import Control.Monad.State
import Data.ITree
import Data.ITree.Zipper (ITreeZipper)
import qualified Data.ITree.Zipper as Z
import           Data.Map      (Map)
import qualified Data.Map      as M

deriving instance Show TypedExp
deriving instance Show ResolvedType

class Expression e where
  checkExpression :: (MonadError String m, MonadState Scope m) => e -> m TypedExp

class Expression a => ResolvedExpression a where
  getType :: a -> ResolvedType
  coerceExpression :: MonadError String m => ResolvedType -> a -> m a
  dereference :: a -> a

class Type t where
  resolve :: (MonadState Scope m, MonadError String m) => t -> m ResolvedType

class Type t => Resolved t where
  coerceTo :: MonadError String m => t -> t -> m t

class Operation o where
  decide :: (MonadError String m, MonadState Scope m) => o -> [TypedExp] -> m TypedExp

data TFunction = TFunction [ResolvedType] ResolvedType
  deriving Show

instance Type TFunction where
  resolve (TFunction args ret) = return ret

checkArgs :: (MonadState Scope m, MonadError String m, ResolvedExpression ex, Resolved t) => [t] -> [ex] -> m [TypedExp]
checkArgs [] [] = return []
checkArgs [] es = throwError "not enough arguments"
checkArgs ts [] = throwError "too many arguments"
checkArgs (t:ts) (e:es) = do
  e' <- checkExpression e
  t' <- resolve t
  e'' <- coerceExpression t' $ dereference e'
  fmap (e'' :) (checkArgs ts es)

data TypedExp where
  DecidedOp :: OperationName -> TFunction -> [TypedExp] -> TypedExp
  Variable :: ResolvedType -> String -> TypedExp
  Narrow :: ResolvedType -> TypedExp -> TypedExp
  Dereference :: ResolvedType -> TypedExp -> TypedExp
  Const :: ResolvedType -> Int -> TypedExp

instance Expression TypedExp where
  checkExpression (DecidedOp ty f@(TFunction types _) args) = DecidedOp ty f <$> checkArgs types args
  checkExpression expr = return expr

instance ResolvedExpression TypedExp where
  getType (DecidedOp _ (TFunction _ ret) _) = ret
  getType (Variable ty _) = ty
  getType (Narrow ty _ ) = ty
  getType (Dereference ty _) = ty
  getType (Const ty _) = ty

  coerceExpression ty@(TSub ty' _ _) expr = Narrow <$> coerceTo ty' ty <*> coerceExpression ty' expr
  coerceExpression ty (DecidedOp name (TFunction argTypes ret) args) = DecidedOp name
    <$> (TFunction argTypes <$> coerceTo ret ty)
    <*> pure args
  coerceExpression ty (Const ty' val) = Const <$> coerceTo ty' ty <*> pure val
  coerceExpression ty (Variable ty' offset) = Variable <$> coerceTo ty' ty <*> pure offset
  coerceExpression ty (Dereference ty' var) = Dereference <$> coerceTo ty' ty <*> pure var

  dereference (Variable (TRef ty) a) = Dereference ty $ Variable (TRef ty) a
  dereference expr = expr

data SymEntry where
  ConstEntry :: Type t => t -> Either TypedExp Int -> SymEntry
  TypeEntry :: Type t => t -> SymEntry
  VarEntry :: Type t => t -> Maybe Int -> SymEntry
  ProcEntry :: Resolved t => [t] -> Maybe Int -> SymEntry

type SymbolTable = Map String SymEntry

data OperationName = Equals
                   | NEquals
                   | LEquals
                   | Less
                   | Greater
                   | GEquals
                   | Plus
                   | Minus
                   | Times
                   | Divide
                   | Negate
                   deriving Show

data ResolvedType where
  TRef :: ResolvedType -> ResolvedType
  TInt :: ResolvedType
  TBool :: ResolvedType
  TSub :: ResolvedType -> TypedExp -> TypedExp -> ResolvedType

instance Type ResolvedType where
  resolve = return

instance Resolved ResolvedType where
  coerceTo (TRef ty) (TRef ty') = coerceTo ty ty'
  coerceTo TInt TInt = return TInt
  coerceTo TInt ty@(TSub ty' f t) = TSub <$> coerceTo ty ty' <*> pure f <*> pure t
  coerceTo TBool TBool = return TBool
  coerceTo (TSub ty _ _) (TSub ty' f t) = TSub <$> coerceTo ty ty' <*> pure f <*> pure t
  coerceTo (TSub ty _ _) ty' = coerceTo ty ty'
  coerceTo ty ty' = throwError $ "cannot coerce " ++ show ty ++ " to " ++ show ty'

emptySymbolTable :: SymbolTable
emptySymbolTable = M.fromList [
  ("int",TypeEntry TInt)
  , ("boolean",TypeEntry TBool)
  ]

newtype Scope = Scope { unScope :: ITreeZipper String SymbolTable }

topLevelScope :: Scope
topLevelScope = Scope . Z.zipITree $ itree emptySymbolTable M.empty

extendScope :: String -> Scope -> Scope
extendScope str (Scope sc) = Scope . Z.down str $ Z.update (setLeaf str M.empty) sc

addEntry :: String -> SymEntry -> Scope -> Scope
addEntry name entry (Scope sc) = Scope $ Z.update (fmap $ M.insert name entry) sc

findEntry :: String -> Scope -> Maybe SymEntry
findEntry name (Scope sc)
  | Z.isTop sc = result
  | otherwise = case result of
    Nothing -> findEntry name (Scope $ Z.up sc)
    s       -> s
  where
    result = M.lookup name $ Z.extractValue sc

enterScope :: String -> Scope -> Scope
enterScope name (Scope sc) = Scope $ Z.down name sc

leaveScope :: Scope -> Scope
leaveScope (Scope sc) = Scope $ Z.up sc
