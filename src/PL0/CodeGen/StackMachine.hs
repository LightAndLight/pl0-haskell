{-# LANGUAGE FlexibleContexts #-}

module PL0.CodeGen.StackMachine (
  instructions
  , generate
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

data StackMachineCode = StackMachineCode { getSize :: Int, instructions :: [Instruction] }

instance Monoid StackMachineCode where
  mempty = StackMachineCode 0 []
  mappend (StackMachineCode s m) (StackMachineCode s' n) = StackMachineCode (s + s') $ m ++ n

toCode :: [Instruction] -> StackMachineCode
toCode is = StackMachineCode (length is) is

singleton :: Instruction -> StackMachineCode
singleton a = toCode [a]

instance Code StackMachineCode where
  generate (Tree block) = (toCode [ZERO, ZERO, ZERO] <>) <$> liftA2 mappend (genBlock block) (pure . toCode $ [ZERO,STOP])

  genBlock (Block decs st) = liftA2 mappend (genDeclarations decs) (genStatement st)

  genAlloc n = toCode [LOAD_CON n, ALLOC_STACK]

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
  genStatement (While cond st) = undefined
  genStatement (If cond th el) = do
    ccond <- genExpression cond
    cthen <- genStatement th
    celse <- genStatement el
    let cthen' = cthen <> toCode [LOAD_CON $ getSize celse, BR]
    return $ ccond
      <> toCode [LOAD_CON $ getSize cthen', BR_FALSE]
      <> cthen'
      <> celse

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
            liftA2 mappend (genExpression expr) (pure $ toCode [LOAD_CON (n + 3), STORE_FRAME])
          Just (ConstEntry ty _) -> error $ "toCodegen: " ++ name ++ " (const) already has an address"
          Just _ -> error $ "toCodegen: " ++ name ++ " not a constant"
          Nothing -> error $ "toCodegen: " ++ name ++ " not found"
      genDec n (VarDecl name _) = do
        entry <- findEntry name <$> get
        case entry of
          Just (VarEntry ty Nothing) -> do
            modify (addEntry name (VarEntry ty (Just $ n + 3)))
            return mempty
          Just (VarEntry ty _) -> error $ "toCodegen: " ++ name ++ " (var) already has an address"
          Just _ -> error $ "toCodegen: " ++ name ++ " not a Variable"
          Nothing -> error $ "toCodegen: " ++ name ++ " not found"
      genDec n (ProcedureDef name params block) = undefined
      genDec n _ = return mempty

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

  genExpression (DecidedOp op _ args) = liftA2 mappend (fold <$> traverse genExpression args) (pure $ genOp op)
  genExpression (Variable _ name) = do
    entry <- findEntry name <$> get
    case entry of
      Just (VarEntry _ (Just addr)) -> pure (singleton $ LOAD_CON addr)
      Just (VarEntry _ Nothing) -> error $ "toCodegen: address for " ++ name ++ " not set"
      Just _ -> error $ "toCodegen: " ++ name ++ " not a variable"
      Nothing -> error $ "toCodegen: " ++ name ++ " not found"
  genExpression (Narrow (TSub _ from to) expr) = do
    cexpr <- genExpression expr
    cfrom <- genExpression from
    cto <- genExpression to
    return $ cexpr <> cfrom <> cto <> singleton BOUND
  genExpression (Dereference _ expr) = liftA2 mappend (genExpression expr) (pure $ singleton LOAD_FRAME)
  genExpression (Const _ val) = return . singleton $ LOAD_CON val
