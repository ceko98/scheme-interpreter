module Primitives (
  primitives
) where

import TypeInstances
import Control.Applicative ( liftA2 )

type Function = [Maybe Value] -> Maybe Value
type Primitives = [(String, Function)]

primitives :: Primitives
primitives = [("plus", numericOp (+))
         ,("minus", numericOp (-))
         ,("lt", boolOp (<))
         ,("gt", boolOp (>))
         ,("eq", eq)
         ,("car", car)
         ,("cdr", cdr)]

eq :: [Maybe Value] -> Maybe Value
eq [(Just x), (Just y)] = Just $ Bool $ x == y
eq _ = Nothing

numericOp :: (Value -> Value -> Value) -> [Maybe Value] -> Maybe Value
numericOp op = foldl1 (liftA2 op)

boolOp :: (Value -> Value -> Bool) -> [Maybe Value] -> Maybe Value
boolOp op [x, y] = fmap Bool $ liftA2 op x y
boolOp _ _ = Nothing

car :: [Maybe Value] -> Maybe Value
car [(Just (List (x:xs)))] = Just x
car _ = Nothing

cdr :: [Maybe Value] -> Maybe Value
cdr [(Just (List (_:xs)))] = Just $ List xs
cdr _ = Nothing
