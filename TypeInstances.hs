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
  deriving (Eq, Ord, Show)
