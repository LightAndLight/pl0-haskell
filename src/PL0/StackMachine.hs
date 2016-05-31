{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PL0.StackMachine where

import           PL0.StackMachine.Instruction

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array
import           Data.Char
import           Data.Bits
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

data MachineState = MachineState { getFramePointer :: Int, getProgramCounter :: Int, getStack :: Stack }

data MachineError = ValueOutOfRange Int Int Int
                  | PCOutOfRange Int
                  | AddressOutOfRange Int
                  | StackOverflow
                  | StackUnderflow
                  | DivByZero
                  | ReadNonInt
                  deriving (Eq, Show)

type Program = Array Int Instruction

newtype StackMachine a = StackMachine {
  runStackMachine :: ReaderT Program (ExceptT MachineError (StateT MachineState IO)) a
} deriving (
  Functor, Applicative, Monad,
  MonadReader Program,
  MonadState MachineState,
  MonadError MachineError,
  MonadIO
  )

data Stack = Stack { getStackPointer :: Int, getStackData :: IOVector Int }

newStack :: MonadIO m => Int -> m Stack
newStack size = Stack (-1) <$> liftIO (V.new size)

push :: (MonadIO m, MonadState MachineState m, MonadError MachineError m) => Int -> m ()
push a = do
  (MachineState fp pc (Stack sp vect)) <- get
  let sp' = sp + 1
  if sp' >= V.length vect
    then throwError StackOverflow
    else do
      liftIO $ V.write vect sp' a
      put $ MachineState fp pc (Stack sp' vect)

pop :: (MonadIO m, MonadError MachineError m, MonadState MachineState m) => m Int
pop = do
  (MachineState fp pc (Stack sp vect)) <- get
  if sp < 0
    then throwError StackUnderflow
    else do
      a <- liftIO $ V.read vect sp
      put $ MachineState fp pc (Stack (sp - 1) vect)
      return a

peek :: (MonadIO m, MonadError MachineError m, MonadState MachineState m) => m Int
peek = do
  (MachineState _ _ (Stack current vect)) <- get
  liftIO $ V.read vect current

incPC :: MonadState MachineState m => m ()
incPC = modify $ \(MachineState fp pc st) -> MachineState fp (pc + 1) st

runProgramFrom :: Int -> Int -> [Instruction] -> IO (Either MachineError Int)
runProgramFrom pc stackSize program = do
  vect <- newStack stackSize
  flip evalStateT (MachineState 0 pc vect)
    . runExceptT
    . flip runReaderT (listArray (0,length program) program)
    . runStackMachine $ interpreter

type MonadMachine m = (MonadState MachineState m, MonadReader Program m, MonadError MachineError m, MonadIO m)

runOnMachine :: MonadMachine m => (Int -> Int -> Int) -> m ()
runOnMachine f = do
  a <- pop
  b <- pop
  push $ f a b

runOnMachineBool :: MonadMachine m => (Int -> Int -> Bool) -> m ()
runOnMachineBool f = do
  a <- pop
  b <- pop
  if f a b then push 1 else push 2

interpreter :: MonadMachine m => m Int
interpreter = do
  (MachineState fp pc st) <- get
  program <- ask
  let (from,to) = bounds program
  if from > pc || pc > to
    then throwError $ PCOutOfRange pc
    else do
      incPC
      case program ! pc of
        NO_OP -> interpreter
        BR -> do
          offset <- pop
          modify $ \(MachineState fp pc st) -> MachineState fp (pc - offset) st
          interpreter
        BR_FALSE -> do
          offset <- pop
          n <- pop
          case n of
            0 -> modify $ \(MachineState fp pc st) -> MachineState fp (pc - offset) st
            _ -> return ()
          interpreter
        COPY -> do
          size <- pop
          to <- pop
          from <- pop
          st <- getStackData . getStack <$> get
          let copy n = do
                aVal <- liftIO $ V.read st (from + n)
                liftIO $ V.write st (to + n) aVal
          traverse copy [0..size-1]
          interpreter
        CALL -> do
          addr <- pop
          if addr < from || to < addr
            then throwError $ AddressOutOfRange addr
            else do
              mstate <- get
              push $ getFramePointer mstate
              push $ getProgramCounter mstate
              (MachineState _ _ st) <- get
              put $ MachineState (getStackPointer st - 2) addr st
              interpreter
        RETURN -> do
          (MachineState fp pc st) <- get
          put $ MachineState fp pc st { getStackPointer = fp + 3 }
          pc' <- pop
          fp' <- pop
          pop
          modify $ \(MachineState _ _ st') -> MachineState fp' pc' st'
          interpreter
        ALLOC_STACK -> do
          n <- pop
          replicateM_ n (push 0x80808080)
          interpreter
        DEALLOC_STACK -> do
          n <- pop
          replicateM_ n pop
          interpreter
        POP -> pop >> interpreter
        DUP -> do
          n <- peek
          push n
          interpreter
        SWAP -> do
          a <- pop
          b <- pop
          push a
          push b
          interpreter
        ADD -> runOnMachine (+) >> interpreter
        MPY -> runOnMachine (*) >> interpreter
        DIV -> do
          b <- pop
          a <- pop
          case b of
            0 -> throwError DivByZero
            _ -> push (a `div` b) >> interpreter
        OR -> runOnMachine (.|.) >> interpreter
        AND -> runOnMachine (.&.) >> interpreter
        XOR -> runOnMachine xor >> interpreter
        EQUAL -> do
          a <- pop
          b <- pop
          if a == b then push 1 else push 0
          interpreter
        LESS -> do
          a <- pop
          b <- pop
          if a < b then push 1 else push 0
          interpreter
        LESSEQ -> do
          a <- pop
          b <- pop
          push $ a + b
          interpreter
        NOT -> do
          a <- pop
          push $ complement a
          interpreter
        NEGATE -> do
          a <- pop
          push (-a)
          interpreter
        READ -> do
          str <- liftIO getLine
          if all isDigit str
            then push (read str) >> interpreter
            else throwError ReadNonInt
        WRITE -> do
          a <- pop
          liftIO $ print a
          interpreter
        BOUND -> do
          upper <- pop
          lower <- pop
          value <- peek
          if lower <= value && value <= upper
            then interpreter
            else throwError $ ValueOutOfRange value lower upper
        TO_GLOBAL -> do
          mstate <- get
          addr <- pop
          push $ addr + getFramePointer mstate
          interpreter
        TO_LOCAL -> do
          mstate <- get
          addr <- pop
          push $ addr - getFramePointer mstate
          interpreter
        LOAD_CON c -> push c >> interpreter
        LOAD_ABS -> do
          addr <- pop
          (MachineState fp pc (Stack current vect)) <- get
          if addr < 0 || V.length vect <= addr
            then throwError $ AddressOutOfRange addr
            else do
              value <- liftIO $ V.read vect addr
              push value
              interpreter
        STORE_FRAME -> do
          offset <- pop
          value <- pop
          (MachineState fp pc (Stack current vect)) <- get
          let location = fp + offset
          if location < 0 || location > V.length vect
            then throwError $ AddressOutOfRange location
            else do
              liftIO $ V.write vect location value
              interpreter
        LOAD_FRAME -> do
          offset <- pop
          (MachineState fp pc (Stack current vect)) <- get
          let location = fp + offset
          if location < 0 || location >= V.length vect
            then throwError $ AddressOutOfRange location
            else do
              value <- liftIO $ V.read vect location
              push value
              interpreter
        ZERO -> push 0 >> interpreter
        ONE -> push 1 >> interpreter
        ALLOC_HEAP -> undefined
        LOAD_MULTI -> do
          (MachineState fp _ (Stack _ vect)) <- get
          count <- pop
          offset <- pop
          let readOne n = liftIO (V.read vect $ fp + offset + n) >>= push
          traverse readOne [0..count]
          interpreter
        STORE_MULTI -> do
          (MachineState fp _ (Stack _ vect)) <- get
          count <- pop
          offset <- pop
          let store' n = do
                value <- pop
                liftIO $ V.write vect (fp + offset + n - 1) value
              store 0 = store' 0
              store n = store' n >> store (n - 1)
          store count
          interpreter
        STOP -> pop
