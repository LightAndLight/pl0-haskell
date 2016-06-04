{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Control.Lens                 hiding (Const)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable
import           Data.Monoid

data StackMachineCode = StackMachineCode {
  _size           :: Int
  , _instructions :: [Instruction]
  } deriving Show

makeLenses ''StackMachineCode

instance Monoid StackMachineCode where
  mempty = StackMachineCode 0 []
  mappend (StackMachineCode s m) (StackMachineCode s' n) = StackMachineCode (s + s') $ m ++ n

toCode :: [Instruction] -> StackMachineCode
toCode is = StackMachineCode (length is) is

singleton :: Instruction -> StackMachineCode
singleton a = toCode [a]

instance Code StackMachineCode where
  generate (Tree block) = do
    mainCode .= toCode [ZERO, ZERO, ZERO]
    cblock <- genBlock block
    mainCode %= flip mappend cblock
    mainCode %= flip mappend (toCode [ZERO,STOP])
    use mainCode

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

  genDeclarations [] = return mempty
  genDeclarations decs = mappend (genAlloc $ length decs) <$> genDecs 0 decs
    where
      genDecs n (d:ds) = liftA2 mappend (genDec n d) (genDecs (n + 1) ds)
      genDecs n [] = return mempty
      genDec n (ConstDef name e) = do
        entry <- findEntry name <$> use scope
        case entry of
          Just (ConstEntry ty (Left expr)) -> do
            scope %= addEntry name (ConstEntry ty (Right $ n + 3))
            liftA2 mappend (genExpression expr) (pure $ toCode [LOAD_CON (n + 3), STORE_FRAME])
          Just (ConstEntry ty _) -> error $ "codegen: " ++ name ++ " (const) already has an address"
          Just _ -> error $ "codegen: " ++ name ++ " not a constant"
          Nothing -> error $ "codegen: " ++ name ++ " not found"
      genDec n (VarDecl name _) = do
        entry <- findEntry name <$> use scope
        case entry of
          Just (VarEntry ty Nothing) -> do
            scope %= addEntry name (VarEntry ty (Just $ n + 3))
            return mempty
          Just (VarEntry ty _) -> error $ "codegen: " ++ name ++ " (var) already has an address"
          Just _ -> error $ "codegen: " ++ name ++ " not a Variable"
          Nothing -> error $ "codegen: " ++ name ++ " not found"
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
    entry <- findEntry name <$> use scope
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
