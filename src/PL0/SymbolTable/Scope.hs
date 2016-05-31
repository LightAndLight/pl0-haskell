module PL0.SymbolTable.Scope (
  Scope
  , topLevelScope
  , getLevel
  , extendScope
  , addEntry
  , findEntry
) where

import           PL0.Internal
