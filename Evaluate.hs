module Evaluate (
  eval,
  Defines
) where

import Primitives
import TypeInstances

type Defines = [(String, Value)]

apply :: Defines -> Value -> [Maybe Value] -> Maybe Value
apply fs (Name func) args = case lookup func primitives of
  Nothing -> applyFunc fs func args
  (Just f) -> f args
apply _ _ _ = Nothing

applyFunc :: Defines -> String -> [Maybe Value] -> Maybe Value
applyFunc fs func args = case lookup func fs of
  Nothing -> Nothing
  (Just f) -> let env = bindVals (getArgs f) args in
    eval fs (replaceWithVals env $ getBody f)

replaceWithVals :: Env -> [Value] -> Maybe Value
replaceWithVals _ [] = Nothing
replaceWithVals env (x:xs) = fmap Scope $ sequence $ (Just x) : (map replace xs) 
  where
    replace :: Value -> Maybe Value
    replace (Name a) = lookup a env
    replace (Scope vals) = replaceWithVals env vals
    replace a = Just a

bindVals :: [Value] -> [Maybe Value] -> Env
bindVals [] _ = []
bindVals _ [] = []
bindVals (Name a : vars) (Just x : xs) = (a, x) : bindVals vars xs
bindVals _ _ = []

eval :: Defines -> Maybe Value -> Maybe Value
eval _ (Just (Scope [Name "define", Scope (Name f : arg), Scope body])) =
  (Just $ Function f arg body [])
eval fs (Just (Scope [Name "if", cond, true, false])) =
  case eval fs $ Just cond of
    Just (Bool True) -> eval fs $ Just true
    _ -> eval fs $ Just false
eval fs (Just (Scope (name : xs))) = apply fs name $ map (eval fs . Just) xs
eval _ (Just x) = Just x
eval _ Nothing = Nothing