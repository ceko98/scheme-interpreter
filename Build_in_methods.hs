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

type Env = [(String, Maybe Value)]

data Value
  = Bool Bool
  | Number Integer
  | List [Value]
  | Name String
  | Scope [Value]
  | Function String [Value] [Value] Env
  deriving Show

instance Semigroup Value where
  Number a <> Number b = Number $ a + b
  _ <> _ = Number 0

-- instance Monoid Value where
--   mappend (Number x) (Number y) = Number $ x + y
--   mappend _ _ = Number 0
--   mempty = Number 0

class Sum a where
  (<+>) :: a -> a -> a

class Prod a where
  (<*>) :: a -> a -> a

instance Sum Value where
  Number a <+> Number b = Number $ a + b
  _ <+> _ = Number 0

instance Prod Value where
  Number a <*> Number b = Number $ a * b
  _ <*> _ = Number 1

instance (Prod а) => Prod (Maybe а) where
  Just a <*> Just b = Just $ a <*> b
  _ <*> _ = Nothing

instance Num Value where
  Number a * Number b = Number $ a * b
  Number a + Number b = Number $ a + b
  Number a - Number b = Number $ a - b

instance Enum Value where
  succ (Number a) = Number $ a + 1
  pred (Number a) = Number $ a - 1

-- instance Integral Value where
--   Number a `div` Number b = Number $ a `div` b

instance Eq Value where
  Number a == Number b = a == b
  Bool a == Bool b = a == b
  x /= y = not $ x == y

-- instance Functor Value where
--   fmap f (Number a) (Number b) = Number $ f a b

-- instance (Sum а) => Semigroup (Maybe а) where
--   (<>) (Just a) (Just b) = Just $ a <+> b
--   (<>) _ _ = Nothing
  
-- instance (Semigroup а) => Monoid (Maybe а) where
--   mappend (Just a) (Just b) = Just $ a <+> b
--   mappend _ _ = Nothing
--   mempty = Nothing

-- instance (Sum a) => Monoid (Maybe a)

