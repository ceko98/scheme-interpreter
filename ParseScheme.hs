{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

module ParseScheme (
  valueParser
) where

import Parser
  ( Parser, nom
  , parse
  , result, empty, (<|>)
  , many, some
  )

import Data.Char (isNumber, isLetter)

import Data.Maybe (maybeToList)

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
sepBy del p = many $ ignoreRight p (optional del)

trueParser :: Parser Value
trueParser = Bool True <$ string "#t"

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

wordParser :: Parser Value
wordParser = do
  str <- some $ parseMaybe $ isChar isLetter
  result $ Name str

optional :: Parser a -> Parser (Maybe a)
optional px = fmap Just px <|> result Nothing

scopeParser :: Parser Value
scopeParser = Scope <$> (between (char '(') (char ')') $ sepBy (char ' ') valueParser)

valueParser :: Parser Value
valueParser =
      boolParser
  <|> numberParser
  <|> wordParser
  <|> scopeParser
