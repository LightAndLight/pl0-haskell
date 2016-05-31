{-# LANGUAGE FlexibleContexts #-}

module PL0.CodeGen.StackMachine (
  generate
  , getStackMachineCode
) where

import           PL0.AST
import           PL0.AST.Class
import           PL0.CodeGen.Class
import           PL0.StackMachine
import           PL0.StackMachine.Instruction
import           PL0.SymbolTable
import           PL0.SymbolTable.Scope

import           Control.Applicative          (liftA2)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable
import           Data.Monoid

data StackMachineCode = StackMachineCode { getSize :: Int, getStackMachineCode :: [Instruction] }

singleton :: Instruction -> StackMachineCode
singleton a = StackMachineCode 1 [a]

cons :: Instruction -> StackMachineCode -> StackMachineCode
cons a (StackMachineCode s mc) = StackMachineCode (s + 1) $ a : mc

prepend :: [Instruction] -> StackMachineCode -> StackMachineCode
prepend as (StackMachineCode n mc) = StackMachineCode (n + length as) $ as ++ mc

instance Monoid StackMachineCode where
  mempty = StackMachineCode 0 []
  mappend (StackMachineCode s m) (StackMachineCode s' n) = StackMachineCode (s + s') $ m ++ n

instance Code StackMachineCode where
  generate (Tree block) = prepend [ZERO, ZERO, ZERO] <$> liftA2 mappend (genBlock block) (pure $ StackMachineCode 2 [ZERO,STOP])

  genBlock (Block decs st) = liftA2 mappend (genDeclarations decs) (genStatement st)

  genAlloc n = StackMachineCode 2 [LOAD_CON n, ALLOC_STACK]

  genStatement (Assignment lvalue expr) = do
    ecode <- genExpression expr
    lcode <- genExpression lvalue
    return $ ecode <> lcode <> singleton STORE_FRAME
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
  genStatement (While cond st) = undefined
  genStatement (If cond th el) = undefined
  genStatement (CallStatement name args) = undefined
  genStatement (Compound sts) = fold <$> traverse genStatement sts
  genStatement SError = throwError "called genStatement on SError"

  genDeclarations [] = return mempty
  genDeclarations decs = mappend (genAlloc $ length decs) <$> genDecs 0 decs
    where
      genDecs n (d:ds) = liftA2 mappend (genDec n d) (genDecs (n + 1) ds)
      genDecs n [] = return mempty
      genDec n (ConstDef name e) = do
        entry <- findEntry name <$> get
        case entry of
          Just (ConstEntry ty (Left expr)) -> do
            modify (addEntry name (ConstEntry ty (Right $ n + 3)))
            liftA2 mappend (genExpression expr) (pure $ StackMachineCode 2 [LOAD_CON (n + 3), STORE_FRAME])
          Just (ConstEntry ty _) -> error $ "codegen: " ++ name ++ " (const) already has an address"
          Just _ -> error $ "codegen: " ++ name ++ " not a constant"
          Nothing -> error $ "codegen: " ++ name ++ " not found"
      genDec n (VarDecl name _) = do
        entry <- findEntry name <$> get
        case entry of
          Just (VarEntry ty Nothing) -> do
            modify (addEntry name (VarEntry ty (Just $ n + 3)))
            return mempty
          Just (VarEntry ty _) -> error $ "codegen: " ++ name ++ " (var) already has an address"
          Just _ -> error $ "codegen: " ++ name ++ " not a Variable"
          Nothing -> error $ "codegen: " ++ name ++ " not found"
      genDec n (ProcedureDef name params block) = undefined
      genDec n _ = return mempty

  genOp Equals = singleton EQUAL
  genOp NEquals = StackMachineCode 4 [EQUAL,NOT,ONE,AND]
  genOp LEquals = singleton LESSEQ
  genOp Less = singleton LESS
  genOp Greater = StackMachineCode 2 [SWAP,LESS]
  genOp GEquals = StackMachineCode 2 [SWAP,LESSEQ]
  genOp Plus = singleton ADD
  genOp Minus = StackMachineCode 2 [NEGATE,ADD]
  genOp Times = singleton MPY
  genOp Divide = singleton DIV
  genOp Negate = singleton NEGATE

  genExpression (DecidedOp op _ args) = liftA2 mappend (fold <$> traverse genExpression args) (pure $ genOp op)
  genExpression (Variable _ name) = do
    entry <- findEntry name <$> get
    case entry of
      Just (VarEntry _ (Just addr)) -> pure (singleton $ LOAD_CON addr)
      Just (VarEntry _ Nothing) -> error $ "codegen: address for " ++ name ++ " not set"
      Just _ -> error $ "codegen: " ++ name ++ " not a variable"
      Nothing -> error $ "codegen: " ++ name ++ " not found"
  genExpression (Narrow (TSub _ from to) expr) = do
    cexpr <- genExpression expr
    cfrom <- genExpression from
    cto <- genExpression to
    return $ cexpr <> cfrom <> cto <> singleton BOUND
  genExpression (Dereference _ expr) = liftA2 mappend (genExpression expr) (pure $ singleton LOAD_FRAME)
  genExpression (Const _ val) = return . singleton $ LOAD_CON val
