{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Tests.PL0.StackMachine (stackMachineTests) where

import           PL0.StackMachine
import           PL0.StackMachine.Instruction

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Array
import qualified Data.Vector.Mutable          as V
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property

newtype TestStack a = TestStack { unTestStack :: ExceptT MachineError (StateT MachineState IO) a }
  deriving (
    Functor, Applicative, Monad,
    MonadError MachineError,
    MonadState MachineState,
    MonadIO
  )

newtype Negative = Negative { runNegative :: Int }
  deriving Show

instance Arbitrary Negative where
  arbitrary = Negative <$> suchThat arbitrary (< 0)

data NotIn = NotIn Int Int Int deriving Show

instance Arbitrary NotIn where
  arbitrary = do
    n <- arbitrary
    from <- suchThat arbitrary (> n)
    to <- suchThat arbitrary (< n)
    return $ NotIn n from to

runTestStack :: TestStack a -> IO (Either MachineError a,MachineState)
runTestStack test = do
  st <- newStack 10
  flip runStateT (MachineState 0 0 st) . runExceptT . unTestStack $ test

prop_push :: Int -> Property
prop_push n = monadicIO $ do
  (_,mstate) <- run . runTestStack $ push n
  let sp = mstate ^. stackPointer
  assert $ sp == 0
  res <- liftIO $ V.read (mstate ^. stackData) sp
  assert $ res == n

prop_peek :: Int -> Property
prop_peek n = monadicIO $ do
  (res,mstate) <- run . runTestStack $ do
    push n
    peek
  assert $ res == Right n
  assert $ mstate ^. stackPointer == 0

prop_pop :: Int -> Property
prop_pop n = monadicIO $ do
  (res,mstate) <- run . runTestStack $ do
    push n
    pop
  assert $ res == Right n
  assert $ mstate ^. stackPointer == -1

prop_negative_pop_error :: Negative -> Property
prop_negative_pop_error (Negative sp) = monadicIO $ do
  (res,_) <- run . runTestStack $ do
    stackPointer .= sp
    pop
  assert $ res == Left StackUnderflow

prop_div_by_zero_error :: Int -> Property
prop_div_by_zero_error n = monadicIO $ do
  res <- run . runProgramFrom 0 10 $ [LOAD_CON n, ZERO, DIV, STOP]
  assert $ res == Left DivByZero

prop_range_error :: NotIn -> Property
prop_range_error (NotIn n from to) = monadicIO $ do
  res <- run . runProgramFrom 0 10 $ [LOAD_CON n, LOAD_CON from, LOAD_CON to, BOUND, STOP]
  assert $ res == Left (ValueOutOfRange n from to)

return []
stackMachineTests = $quickCheckAll
