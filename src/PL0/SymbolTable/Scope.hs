module PL0.SymbolTable.Scope (
  HasScope(..)
  , Scope
  , topLevelScope
  , extendScope
  , enterScope
  , leaveScope
  , addEntry
  , findEntry
  , getTable
  , getScopeName
) where

import           PL0.Internal
