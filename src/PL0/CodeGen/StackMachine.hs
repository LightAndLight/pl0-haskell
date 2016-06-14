{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

module PL0.CodeGen.StackMachine (
  generate
  , initialState
) where

import           PL0.AST
import           PL0.AST.Class
import           PL0.StackMachine
import           PL0.StackMachine.Instruction
import           PL0.SymbolTable
import           PL0.SymbolTable.Scope

import           Control.Applicative          (liftA2)
import           Control.Lens                 hiding (Const)
import           Control.Monad.Except
import           Control.Monad.State hiding (put)
import Data.ByteString (ByteString)
import           Data.Foldable
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Serialize

data Code = Code {
  _size           :: Int
  , _instructions :: [Instruction]
  } deriving Show

makeClassy ''Code

instance Monoid Code where
  mempty = Code 0 []
  mappend (Code s m) (Code s' n) = Code (s + s') $ m ++ n

toCode :: [Instruction] -> Code
toCode is = Code (length is) is

singleton :: Instruction -> Code
singleton a = toCode [a]

data CodeGenState = CodeGenState {
  _cgScope      :: Scope
  , _procedures :: [Declaration TypedExp]
  , _cgCode     :: Code
  }

initialState :: CodeGenState
initialState = CodeGenState topLevelScope [] mempty

makeClassy ''CodeGenState

instance HasScope CodeGenState where
  scope = cgScope
  locals = cgScope . locals

instance HasCode CodeGenState where
  code = cgCode
  size = cgCode . size
  instructions = cgCode . instructions

type MonadCode s m = (
  HasCode s
  , HasCodeGenState s
  , HasScope s
  , MonadError String m
  , MonadState s m
  )

generate :: MonadCode s m => Tree TypedExp -> m Program
generate (Tree (Block decs st)) = do
  code .= mempty
  genProcedures decs
  entry <- use size
  code <>= toCode [ZERO,ZERO]
  genDeclarations decs
  cst <- genStatement st
  code <>= cst
  code <>= toCode [ZERO,STOP]
  insts <- use instructions
  return $ Program entry insts

genBlock :: MonadCode s m => Block TypedExp -> m ()
genBlock (Block decs st) = do
  procedures .= []
  genDeclarations decs
  genProcedures decs
  cst <- genStatement st
  code <>= cst

genAlloc :: Int -> Code
genAlloc 0 = singleton NO_OP
genAlloc 1 = singleton $ LOAD_CON 0x80808080
genAlloc n = toCode [LOAD_CON n, ALLOC_STACK]

genExpression :: MonadCode s m => TypedExp -> m Code
genExpression (DecidedOp op _ args) = liftA2 mappend (fold <$> traverse genExpression args) (pure $ genOp op)
genExpression (Variable _ name) = do
  entry <- M.lookup name <$> use (scope . locals)
  case entry of
    Just (VarEntry _ (Just addr)) -> pure (singleton $ LOAD_CON addr)
    Just (VarEntry _ Nothing) -> error $ "codegen: address for " ++ name ++ " not set"
    Just _ -> error $ "codegen: " ++ name ++ " not a variable"
    Nothing -> mappend (toCode [ZERO,LOAD_FRAME,LOAD_ABS]) <$> searchStackFrames name
  where
    searchStackFrames name = do
      scopeName <- getScopeName <$> use scope
      scope %= leaveScope
      entry <- M.lookup name <$> use (scope . locals)
      code <- case entry of
        Just (VarEntry _ (Just addr)) -> pure $ toCode [LOAD_CON addr,ADD,TO_LOCAL]
        Just (VarEntry _ Nothing) -> error $ "codegen: address for " ++ name ++ " not set"
        Just _ -> error $ "codegen: " ++ name ++ " not a variable"
        Nothing -> mappend (singleton LOAD_ABS) <$> searchStackFrames name
      case scopeName of
        Just sn -> scope %= enterScope sn
        Nothing -> return ()
      return code
genExpression (Narrow (TSub _ from to) expr) = do
  cexpr <- genExpression expr
  cfrom <- genExpression from
  cto <- genExpression to
  return $ cexpr <> cfrom <> cto <> singleton BOUND
genExpression (Dereference _ expr) = liftA2 mappend (genExpression expr) (pure $ singleton LOAD_FRAME)
genExpression (Const _ val) = return . singleton $ LOAD_CON val

genStatement :: MonadCode s m => Statement TypedExp -> m Code
genStatement (Assignment lvalue expr) = do
  etoCode <- genExpression expr
  ltoCode <- genExpression lvalue
  return $ etoCode <> ltoCode <> singleton STORE_FRAME

genStatement (Read e) = do
  ce <- genExpression e
  check <- case getType (dereference e) of
    TSub _ from to -> do
      cfrom <- genExpression from
      cto <- genExpression to
      return $ cfrom <> cto <> singleton BOUND
    _ -> return mempty
  return $ singleton READ <> check <> ce <> singleton STORE_FRAME

genStatement (Write e) = liftA2 mappend (genExpression e) (pure $ singleton WRITE)

genStatement (While cond st) = do
  ccond <- genExpression cond
  cst <- genStatement st
  let ccond' = ccond <> toCode [LOAD_CON $ cst ^. size + 2, BR_FALSE]
  let cst' = cst <> toCode [LOAD_CON $ -(ccond' ^. size) - (cst ^. size + 2), BR]
  return $ ccond' <> cst'

genStatement (If cond th el) = do
  ccond <- genExpression cond
  cthen <- genStatement th
  celse <- genStatement el
  let cthen' = cthen <> toCode [LOAD_CON $ celse ^. size, BR]
  return $ ccond
    <> toCode [LOAD_CON $ cthen' ^. size, BR_FALSE]
    <> cthen'
    <> celse

genStatement (CallStatement name args) = do
  entry <- findEntry name <$> use scope
  case entry of
    Just (ProcEntry params (Just loc)) -> do
      cargs <- fold <$> traverse genExpression (reverse args)
      scope %= enterScope name
      return $ cargs <> toCode [ZERO, LOAD_FRAME, LOAD_CON loc, CALL]
    Just (ProcEntry _ Nothing) -> error $ "codegen: procedure " ++ name ++ " missing address"
    Just _ -> error $ "codegen: entry \"" ++ name ++ "\" not a procedure"
    Nothing -> error $ "codegen: entry \"" ++ name ++ "\" not found"
genStatement (Compound sts) = fold <$> traverse genStatement sts
genStatement SError = throwError "called genStatement on SError"

genDeclarations :: MonadCode s m => [Declaration TypedExp] -> m ()
genDeclarations = genDecs 0
  where
    genDecs n [] = return ()
    genDecs n (ConstDef name e:ds) = do
      entry <- findEntry name <$> use scope
      case entry of
        Just (ConstEntry _ (Right addr)) -> do
          cexpr <- genExpression e
          code <>= cexpr
        Just (ConstEntry ty _) -> error $ "codegen: " ++ name ++ " (const) has no address"
        Just _ -> error $ "codegen: " ++ name ++ " not a constant"
        Nothing -> error $ "codegen: " ++ name ++ " not found"
      genDecs (n + 1) ds
    genDecs n (VarDecl name _:ds) = do
      entry <- findEntry name <$> use scope
      case entry of
        Just (VarEntry ty (Just addr)) -> code <>= genAlloc 1
        Just (VarEntry ty _) -> error $ "codegen: " ++ name ++ " (var) has no address"
        Just _ -> error $ "codegen: " ++ name ++ " not a variable"
        Nothing -> error $ "codegen: " ++ name ++ " not found"
      genDecs (n + 1) ds
    genDecs n (p@ProcedureDef{}:ds) = do
      procedures <>= [p]
      genDecs n ds
    genDecs n (d:ds) = genDecs n ds

genProcedures :: MonadCode s m => [Declaration TypedExp] -> m ()
genProcedures [] = return ()
genProcedures (ProcedureDef name params block:ds) = do
  entry <- findEntry name <$> use scope
  case entry of
    Just (ProcEntry tys Nothing) -> do
      pos <- use $ code . size
      scope %= addEntry name (ProcEntry tys (Just pos))
      scope %= enterScope name
      genBlock block
      code <>= singleton RETURN
      scope %= leaveScope
    Just (ProcEntry _ _) -> error $ "codegen: " ++ name ++ " (procedure) already has an address"
    Just _ -> error $ "codegen: " ++ name ++ " not a procedure"
    Nothing -> error $ "codegen: " ++ name ++ " not found"
  genProcedures ds
genProcedures (d:ds) = genProcedures ds

genOp :: OperationName -> Code
genOp Equals = singleton EQUAL
genOp NEquals = toCode [EQUAL,NOT,ONE,AND]
genOp LEquals = singleton LESSEQ
genOp Less = singleton LESS
genOp Greater = toCode [SWAP,LESS]
genOp GEquals = toCode [SWAP,LESSEQ]
genOp Plus = singleton ADD
genOp Minus = toCode [NEGATE,ADD]
genOp Times = singleton MPY
genOp Divide = singleton DIV
genOp Negate = singleton NEGATE
