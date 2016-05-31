{-# LANGUAGE TemplateHaskell #-}

module Tests.PL0 (pl0Tests) where

import           PL0.Lexer
import           PL0.Parser
import           PL0.StaticChecker

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

prop_compiles_correctly :: Property
prop_compiles_correctly = forAll (return "test.pl0") testFile
  where
    testFile name = monadicIO $ do
      content <- liftIO . readFile $ "test/testfiles/" ++ name
      let tree = scanTokens >=> parseTokens $ content
      assert $ isRight tree

return []
pl0Tests = $quickCheckAll
