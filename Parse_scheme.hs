{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

import Parser
  ( Parser, nom
  , parse
  , result, empty, (<|>)
  , many, some
  )

import Data.Char (isNumber, isLetter)

data Value
  = Bool Bool
  | Number Integer
  | List [Value]
  | Variable (String, Value)
  deriving Show

-- char :: Char -> Parser Char
-- char c = do
--   x <- nom
--   if x == c
--   then result x
--   else empty

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

wordParser :: Parser String
wordParser = do
  str <- many $ parseMaybe $ isChar isLetter
  result str

scopeParser :: Parser [Value]
scopeParser = between (char '(') (char ')') $ sepBy (char ' ') (boolParser <|> numberParser <|> listParser) <|> result []

valueParser :: Parser Value
valueParser = boolParser
  <|> numberParser
  -- <|> scopeParser