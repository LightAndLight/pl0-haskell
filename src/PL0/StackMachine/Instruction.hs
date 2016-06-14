{-# LANGUAGE DeriveGeneric #-}

module PL0.StackMachine.Instruction where

import           Data.Serialize
import           Data.Word
import           GHC.Generics

data Instruction = NO_OP
                 | BR
                 | BR_FALSE
                 | COPY
                 | CALL
                 | RETURN
                 | ALLOC_STACK
                 | DEALLOC_STACK
                 | POP
                 | DUP
                 | SWAP
                 | ADD
                 | MPY
                 | DIV
                 | OR
                 | AND
                 | XOR
                 | EQUAL
                 | LESS
                 | LESSEQ
                 | NOT
                 | NEGATE
                 | READ
                 | WRITE
                 | BOUND
                 | TO_GLOBAL
                 | TO_LOCAL
                 | LOAD_CON Int
                 | LOAD_ABS
                 | STORE_FRAME
                 | LOAD_FRAME
                 | ZERO
                 | ONE
                 | ALLOC_HEAP
                 | LOAD_MULTI
                 | STORE_MULTI
                 | STOP
                 deriving (Generic,Show)

instance Serialize Instruction where
