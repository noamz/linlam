module LinLam.Parse where

import Data.Char
import Data.Maybe

import LinLam.Core
import LinLam.Typing

import Text.ParserCombinators.Parsec
import qualified Data.Map as M

type ParserLT a = GenParser Char (M.Map String Int) a

parseLT :: ParserLT LT
parseLT = parseLam <|> parseApps

parseLam :: ParserLT LT
parseLam = do
  char '\\'
  spaces
  vname <- manyTill alphaNum (spaces >> char '.')
  m <- getState
  let m' = if M.member vname m then m else M.insert vname (M.size m) m
  setState m'
  t <- parseLT
  let x = fromJust $ M.lookup vname m'
  return $ L x $ t

parseApps :: ParserLT LT
parseApps = do
  h <- parseLT'
  skipMany (char ' ')
  ts <- sepBy parseLT' (skipMany (char ' '))
  return (foldl A h ts)

parseVar :: ParserLT LT
parseVar = do
  vname <- many1 alphaNum
  m <- getState
  let m' = if M.member vname m then m else M.insert vname (M.size m) m
  setState m'
  let x = fromJust $ M.lookup vname m'
  return $ V x

parseLT' :: ParserLT LT
parseLT' = parseVar <|> between (char '(') (char ')') parseLT

parseComment :: ParserLT String
parseComment = char ';' >> manyTill anyChar newline

parseLTs :: ParserLT [LT]
parseLTs = do
  many parseComment
  ts <- sepEndBy parseLT (newline >> many parseComment)
  eof
  return ts

readLT :: String -> LT
readLT w = case runParser parseLT M.empty "" w of
             Right t -> t
             Left err -> error ("readLT: " ++ show err)

readLTsFromFile :: FilePath -> IO [LT]
readLTsFromFile file = do
  input <- readFile file
  case runParser parseLTs M.empty "" input of
    Right ts -> return ts
    Left err -> do
      print err
      error ("readLTsFromFile: parse error")

{-
instance Read LT where
  read = readLT
-}

