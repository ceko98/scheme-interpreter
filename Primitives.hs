module Primitives (
  primitives
) where

import TypeInstances
import Control.Applicative ( liftA2 )

type Function = [Maybe Value] -> Maybe Value
type Primitives = [(String, Function)]

primitives :: Primitives
primitives =
  [ ("plus", numericOp addVal)
  , ("minus", numericOp minusVal)
  , ("lt", boolOp (<))
  , ("gt", boolOp (>))
  , ("eq", eq)
  , ("car", listOp head)
  , ("cdr", listOp (List . tail))
  ]

eq :: [Maybe Value] -> Maybe Value
eq [(Just x), (Just y)] = Just $ Bool $ x == y
eq _ = Nothing

numericOp :: (Value -> Value -> Value) -> [Maybe Value] -> Maybe Value
numericOp op = foldl1 (liftA2 op)

boolOp :: (Value -> Value -> Bool) -> [Maybe Value] -> Maybe Value
boolOp op [x, y] = fmap Bool $ liftA2 op x y
boolOp _ _ = Nothing

listOp :: ([Value] -> Value) -> [Maybe Value] -> Maybe Value
listOp f [(Just (List xs))] = Just $ f xs
listOp _ _ = Nothing

addVal :: Value -> Value -> Value
addVal (Number x) (Number y) = Number $ x + y
addVal _ _ = error "can't add these"

minusVal :: Value -> Value -> Value
minusVal (Number x) (Number y) = Number $ x - y
minusVal _ _ = error "can't minus these"
