{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module TypeInstances (
  Value(..),
  Env
) where

import Prelude hiding ((<*>))
import Data.Monoid hiding (Sum)

type Env = [(String, Value)]

data Value
  = Bool Bool
  | Number Integer
  | List [Value]
  | Name String
  | Scope [Value]
  | Function { getName :: String,
               getArgs :: [Value],
               getBody :: [Value]}
  deriving Show

instance Num Value where
  Number a * Number b = Number $ a * b
  Number a + Number b = Number $ a + b
  Number a - Number b = Number $ a - b

instance Enum Value where
  succ (Number a) = Number $ a + 1
  pred (Number a) = Number $ a - 1

instance Eq Value where
  Number a == Number b = a == b
  Bool a == Bool b = a == b
  x /= y = not $ x == y

instance Ord Value where
  Number a <= Number b = a <= b
  Number a >= Number b = a >= b
  x > y = not $ x <= y
  x < y = not $ x >= y
