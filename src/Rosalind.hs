module Rosalind
( seqsFromString
, idFromString
, fastaFromString
, codonMapFromString
, proteinMassMapFromString )
where

import Text.Trifecta
import Data.List (groupBy)
import Control.Applicative ((<|>))

type ID = String
type Seq = String
type Fasta = [(ID, Seq)]

seqsFromString :: String -> [Seq]
seqsFromString = map snd . fastaFromString

idFromString :: String -> [ID]
idFromString = map fst . fastaFromString

fastaFromString :: String -> Fasta
fastaFromString =
  map (\(x:xs) -> (tail x, concat xs))
    . groupBy (\xs ys -> head xs == '>' && head ys /= '>')
    . lines

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf " \n"

parseCodon :: Parser (String, String)
parseCodon = do
  triplet <- some $ noneOf " \n"
  char ' '
  acid <- (string "Stop" >> string "") <|> some (noneOf " \n")
  skipEOL
  return (triplet, acid)

parseMassEntry :: Parser (Char, Double)
parseMassEntry = do
  acid <- anyChar
  skipEOL
  weight <- double
  skipEOL
  return (acid, weight)

codonMapFromString :: String -> [(String, String)]
codonMapFromString s = case parseString (some parseCodon) mempty s of
  Success x -> x
  Failure _ -> []

proteinMassMapFromString :: String -> [(Char, Double)]
proteinMassMapFromString s = case parseString (some parseMassEntry) mempty s of
  Success x -> x
  Failure _ -> []
