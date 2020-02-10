{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module Parse
  ( isChar, Monoid(..) ) where

import Parser
  ( Parser, nom
  , parse
  , result, empty, (<|>)
  , many, some
  )

import Data.Char (isNumber, isLetter)

import Control.Applicative (liftA2)

import TypeInstances

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- nom
  if p x
  then result x
  else empty

parseMaybe :: (Char -> Maybe a) -> Parser a
parseMaybe f = do
  x <- nom
  case f x of
    Nothing -> empty
    Just a -> result a

-- parseStringMaybe :: (Char -> Maybe a) -> Parser [a]
-- parseStringMaybe = some . parseMaybe

isInteger :: Char -> Maybe Integer
isInteger c = if isNumber c then Just $ read [c] else Nothing

isChar :: (Char -> Bool) -> Char -> Maybe Char
isChar f c = if f c then Just c else Nothing

number :: Parser Integer
number = fmap (foldl ((+) . (10*)) 0) $ some $ parseMaybe isInteger

maybeToParser :: Maybe a -> Parser a 
maybeToParser Nothing = empty
maybeToParser (Just x) = pure x

char :: Char -> Parser Char
char ch = parseMaybe $ isChar (ch==)

string :: String -> Parser String
string [] = result []
string (s:str) = do
  x <- parseMaybe $ isChar (s==)
  xs <- string str
  result $ x:xs

surround :: Parser del -> Parser a -> Parser a
surround del p = do
  del
  x <- p
  del
  result x

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  open
  x <- p
  close
  result x

ignoreRight :: Parser a -> Parser b -> Parser a
ignoreRight p ign = do
  x <- p
  ign
  result x

sepBy :: Parser del -> Parser a -> Parser [a]
sepBy del p = do
  xs <- many (ignoreRight p del)
  x <- p
  result $ xs ++ [x]

trueParser :: Parser Value
trueParser = do
  string "#t"
  result $ Bool True

falseParser :: Parser Value
falseParser = do
  string "#f"
  result $ Bool False

boolParser :: Parser Value
boolParser = trueParser <|> falseParser

numberParser :: Parser Value
numberParser = do
  x <- number
  result $ Number x

listParser :: Parser Value
listParser = do
  char '\''
  xs <- between (char '(') (char ')') $ sepBy (char ' ') (boolParser <|> numberParser <|> listParser) <|> result []
  result $ List xs

wordParser :: Parser Value
wordParser = do
  str <- some $ parseMaybe $ isChar isLetter
  result $ Name str

scopeParser :: Parser Value
scopeParser = do
  xs <- between (char '(') (char ')') $ sepBy (char ' ') (boolParser <|> numberParser <|> listParser <|> wordParser <|> scopeParser) <|> result []
  result $ Scope xs

valueParser :: Parser Value
valueParser = boolParser
  <|> numberParser
  <|> listParser
  <|> scopeParser

eq :: [Maybe Value] -> Maybe Value
eq [(Just x), (Just y)] = Just $ Bool $ x == y
eq _ = Nothing

numericOp :: (Value -> Value -> Value) -> [Maybe Value] -> Maybe Value
numericOp op = foldl1 (liftA2 op)

boolOp :: (Value -> Value -> Bool) -> [Maybe Value] -> Maybe Value
boolOp op [x, y] = fmap Bool $ liftA2 op x y
boolOp _ _ = Nothing

if' :: [Maybe Value] -> Maybe Value
if' [(Just (Bool cond)), true, false] = if cond then true else false
if' _ = Nothing

type Defines = [(String, Value)]

run :: Defines -> IO ()
run defines = do
  r <- getLine
  let p = eval defines $ parse valueParser r
  print p
  case p of
    Just (Function name args body env) -> run $ (name, Function name args body env) : defines
    _ -> run defines

type Function = [Maybe Value] -> Maybe Value
type Primitives = [(String, Function)]

primitives :: Primitives
primitives = [("plus", numericOp (+))
         ,("minus", numericOp (-))
         ,("lt", boolOp (<))
         ,("gt", boolOp (>))
         ,("eq", eq)
         ,("if", if')]

apply :: Defines -> Value -> [Maybe Value] -> Maybe Value
apply fs (Name func) args = case lookup func primitives of
  Nothing -> applyFunc fs func args
  (Just f) -> f args
apply _ _ _ = Nothing

applyFunc :: Defines -> String -> [Maybe Value] -> Maybe Value
applyFunc fs func args = case lookup func fs of
  Nothing -> Just $ Name "fails"
  (Just f) -> let env = bindVals (getArgs f) args in
    eval fs (replaceWithVals env $ getBody f)

replaceWithVals :: Env -> [Value] -> Maybe Value
replaceWithVals _ [] = Nothing
replaceWithVals env (x:xs) = fmap Scope $ sequence $ (Just x) : (map replace xs) 
  where
    replace :: Value -> Maybe Value
    replace (Name a) = lookup a env
    replace a = Just a

testFunc :: Value
testFunc = Function "a" [Name "x"] [Name "plus",Name "x",Number 1] []

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
