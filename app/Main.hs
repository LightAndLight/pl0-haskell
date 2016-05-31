module Main where

import           PL0.CodeGen.Class
import           PL0.CodeGen.StackMachine
import           PL0.Lexer
import           PL0.Parser
import           PL0.StackMachine
import           PL0.StaticChecker
import           PL0.SymbolTable.Scope

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
      let tree = scanTokens content >>= parseTokens
      case tree of
        Right tree' -> do
          let mprogram = checkProgram tree' >>= generate
              program = flip evalState topLevelScope . runExceptT $ mprogram
          case program of
            Right program' -> runProgramFrom 0 100 (getStackMachineCode program') >>= print
            Left err -> print err
        Left err -> print err
