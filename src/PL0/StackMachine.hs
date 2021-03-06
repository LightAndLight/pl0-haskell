{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module PL0.StackMachine (
  HasStack(..)
  , MachineError(..)
  , MachineState(..)
  , Program(..)
  , interpreter
  , newStack
  , peek
  , pop
  , push
  , runProgram
) where

import           PL0.StackMachine.Instruction

import           Control.Applicative (liftA2)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array
import Data.ByteString (ByteString)
import           Data.Char
import           Data.Bits
import Data.Serialize hiding (get, put)
import qualified Data.Serialize as B
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V

data Stack = Stack { _stackPointer :: Int, _stackData :: IOVector Int }

data MachineState = MachineState { _framePointer :: Int, _programCounter :: Int, _stackState :: Stack }

makeClassy ''Stack
makeClassy ''MachineState

instance HasStack MachineState where
  stackPointer = stackState . stackPointer
  stackData = stackState . stackData
  stack = stackState

data MachineError = ValueOutOfRange Int Int Int
                  | PCOutOfRange Int
                  | AddressOutOfRange Int
                  | StackOverflow
                  | StackUnderflow
                  | DivByZero
                  | ReadNonInt
                  | InvalidBinary
                  deriving (Eq, Show)

type MonadMachine s m = (
  MonadState s m
  , MonadReader (Array Int Instruction) m
  , MonadError MachineError m
  , MonadIO m
  )

newStack :: MonadIO m => Int -> m Stack
newStack size = Stack (-1) <$> liftIO (V.new size)

checkBounds :: MonadError MachineError m => Int -> IOVector e -> m a -> m a
checkBounds sp sdata m
  | sp < 0 = throwError StackUnderflow
  | sp >= V.length sdata = throwError StackOverflow
  | otherwise = m

push :: (HasStack s, MonadIO m, MonadState s m, MonadError MachineError m) => Int -> m ()
push a = do
  stackPointer += 1
  sp <- use stackPointer
  sdata <- use stackData
  checkBounds sp sdata (liftIO $ V.write sdata sp a)

pop :: (HasStack s, MonadIO m, MonadError MachineError m, MonadState s m) => m Int
pop = do
  sp <- use stackPointer
  sdata <- use stackData
  checkBounds sp sdata $ do
    a <- liftIO $ V.read sdata sp
    stackPointer -= 1
    return a

peek :: (HasStack s, MonadIO m, MonadError MachineError m, MonadState s m) => m Int
peek = do
  sp <- use stackPointer
  sdata <- use stackData
  checkBounds sp sdata (liftIO $ V.read sdata sp)

data Program = Program {
  programEntry :: Int
  , programInstructions :: [Instruction]
}

instance Serialize Program where
  put (Program entry insts) = do
    B.put "STACKM"
    B.put entry
    B.put insts
  get = expect "STACKM" *> pure Program <*> B.get <*> B.get

runProgram :: ByteString -> IO (Either MachineError Int)
runProgram input = case decode input of
  Right (Program entry insts) -> do
    let program = listArray (0,length insts) insts
    vect <- newStack 1000
    flip evalStateT (MachineState 0 entry vect)
      . runExceptT
      . flip runReaderT program
      $ interpreter
  Left err -> return $ Left InvalidBinary

runOnMachine :: (HasStack s, MonadMachine s m) => (Int -> Int -> Int) -> m ()
runOnMachine f = do
  a <- pop
  b <- pop
  push $ f a b

runOnMachineBool :: (HasStack s, MonadMachine s m) => (Int -> Int -> Bool) -> m ()
runOnMachineBool f = do
  a <- pop
  b <- pop
  if f a b then push 1 else push 2

addTopFP :: (HasStack s, HasMachineState s, MonadMachine s m) => m Int
addTopFP = liftA2 (+) (use framePointer) pop

interpreter :: (HasStack s, HasMachineState s, MonadMachine s m) => m Int
interpreter = do
  program <- ask
  pc <- use programCounter
  fp <- use framePointer
  let (from,to) = bounds program
  if from > pc || pc > to
    then throwError $ PCOutOfRange pc
    else do
      programCounter += 1
      case program ! pc of
        NO_OP -> interpreter
        BR -> do
          offset <- pop
          programCounter += offset
          interpreter
        BR_FALSE -> do
          offset <- pop
          n <- pop
          case n of
            0 -> programCounter += offset
            _ -> return ()
          interpreter
        COPY -> do
          size <- pop
          to <- pop
          from <- pop
          st <- use stackData
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
              let link = mstate ^. stackPointer
              push $ mstate ^. framePointer
              push $ mstate ^. programCounter
              framePointer .= link
              programCounter .= addr
              interpreter
        RETURN -> do
          fp <- use framePointer
          stackPointer .= fp + 2
          pc' <- pop
          fp' <- pop
          pop
          programCounter .= pc'
          framePointer .= fp'
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
          b <- pop
          a <- pop
          if a < b then push 1 else push 0
          interpreter
        LESSEQ -> do
          b <- pop
          a <- pop
          if a <= b then push 1 else push 0
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
          addr <- pop
          push $ addr + fp
          interpreter
        TO_LOCAL -> do
          fp <- use framePointer
          addr <- pop
          push $ addr - fp
          interpreter
        LOAD_CON c -> push c >> interpreter
        LOAD_ABS -> do
          addr <- pop
          sdata <- use stackData
          if addr < 0 || V.length sdata <= addr
            then throwError $ AddressOutOfRange addr
            else do
              value <- liftIO $ V.read sdata addr
              push value
              interpreter
        STORE_FRAME -> do
          location <- addTopFP
          value <- pop
          sdata <- use stackData
          sp <- use stackPointer
          if location < 0 || sp < location
            then throwError $ AddressOutOfRange location
            else do
              liftIO $ V.write sdata location value
              interpreter
        LOAD_FRAME -> do
          sdata <- use stackData
          location <- addTopFP
          sp <- use stackPointer
          if location < 0 || sp < location
            then throwError $ AddressOutOfRange location
            else do
              value <- liftIO $ V.read sdata location
              push value
              interpreter
        ZERO -> push 0 >> interpreter
        ONE -> push 1 >> interpreter
        ALLOC_HEAP -> undefined
        LOAD_MULTI -> do
          count <- pop
          location <- addTopFP
          sdata <- use stackData
          let readOne n = liftIO (V.read sdata $ location + n) >>= push
          traverse readOne [0..count]
          interpreter
        STORE_MULTI -> do
          count <- pop
          offset <- pop
          sdata <- use stackData
          fp <- use framePointer
          let store' n = do
                value <- pop
                liftIO $ V.write sdata (fp + offset + n - 1) value
              store 0 = store' 0
              store n = store' n >> store (n - 1)
          store count
          interpreter
        STOP -> pop
