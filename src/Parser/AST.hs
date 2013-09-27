{-# LANGUAGE DeriveDataTypeable #-}
module Parser.AST where

import Control.Lens (Plated)
import Data.Data

data AST =
    IntLit Integer |
    DoubleLit Double |
    AST :+ AST |
    AST :- AST |
    AST :* AST |
    AST :/ AST |
    AST :^ AST
    deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Plated AST

data BinaryOp = Add | Subtract | Multiply | Divide
              deriving (Eq, Ord, Read, Show)
data UnaryOp = Negate | Sqrt
             deriving (Eq, Ord, Read, Show)
