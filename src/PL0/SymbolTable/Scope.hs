module PL0.SymbolTable.Scope (
  Scope
  , topLevelScope
  , extendScope
  , enterScope
  , leaveScope
  , addEntry
  , findEntry
) where

import           PL0.Internal
