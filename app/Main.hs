module Main where

import           PL0.CodeGen.Class
import           PL0.CodeGen.StackMachine
import           PL0.Lexer
import           PL0.Parser
import           PL0.StackMachine
import           PL0.StaticChecker
import           PL0.SymbolTable.Scope

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
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
             >=> generate
      case flip evalState initialState . runExceptT . full $ content of
        Right program' -> runProgramFrom 0 1000 (program' ^. instructions) >>= print
        Left err -> print err
