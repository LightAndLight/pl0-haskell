module Main where

import           PL0.CodeGen.StackMachine
import           PL0.Lexer
import           PL0.Parser
import           PL0.StackMachine
import           PL0.StackMachine.Linker
import           PL0.StaticChecker
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State      (evalStateT)
import qualified Data.ByteString          as B
import           Data.Serialize
import           System.Environment

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Enter a file"
    (x:xs) -> do
      content <- readFile x
      let full = ExceptT . return . scanTokens
             >=> ExceptT . return . parseTokens
             >=> checkProgram
             >=> link
             >=> generate
      flip evalStateT initialState . runExceptT $ do
        program <- full content
        liftIO $ B.writeFile "test.vm" (encode program)
        program' <- liftIO $ B.readFile "test.vm"
        liftIO $ runProgram program'
      return ()
