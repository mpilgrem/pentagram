{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Main (main) where

import Paths_pentagram (getDataFileName)
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, (<++), between, char, many,
  readP_to_S, satisfy)

-- From package containers
import Data.Set (Set, empty, fromList, notMember, singleton)

-- Type synonym representing known states of a Wordle tile (yellow
-- characters, maybe green character).
type WordleTile = (Set Char, Maybe Char)

-- Type representing known Wordle states: Wordle tiles, greyed-out
-- characters.
data WordleState = WordleState [WordleTile] (Set Char)
  deriving Show

instance Semigroup WordleState where

  WordleState wts1 gts1 <> WordleState wts2 gts2 =
    WordleState (zipWith (<>) wts1 wts2) (gts1 <> gts2)

-- By defining an instance of Semigroup for Char, avoids the need to define an
-- instance for WordleTile.
instance Semigroup Char where

  c1 <> c2 = if c1 == c2
               then c1
               else error "Inconsistent Wordle tiles"

-- The first five arguments are the Wordle tile states and the sixth is the
-- greyed-out letters (use '.' for none).
main :: IO ()
main = do
  args <- getArgs
  setBWords <- words <$> (readFile =<< getDataFileName "setb.wordlist")
  wordleWords <- words <$> (readFile =<< getDataFileName "Wordle.wordlist")
  let colourTiles = map (fst . head . readP_to_S readWordleTile) $ take 5 args
      greyTiles = fromList $ args !! 5
      ws = WordleState colourTiles greyTiles
      f = mkFilter ws
      setBWords' = filter (`notElem` wordleWords) setBWords
      result = filter f setBWords'
      ks' = map (\r -> (fst r, knowledge setBWords' ws r)) (choices result)
      ks = sortOn snd ks'
  print ks

-- Function to create a filter (String -> Bool) from a known Wordle state
mkFilter :: WordleState -> String -> Bool
mkFilter (WordleState wordleTiles greyTiles) w =
  and $ zipWith filterChar wordleTiles (choices w)
 where
  filterChar :: WordleTile -> (Char, [Char]) -> Bool
  filterChar (yellowChars, mGreenTile) (c, others) =
    c `notMember` greyTiles &&
    maybe True (c ==) mGreenTile &&
    c `notMember` yellowChars &&
    all (`elem` others) yellowChars

-- Function to count possible further outcomes, given a list of five-letter
-- words, a known Wordle state, a guess and a list of possible solutions.
knowledge :: [String] -> WordleState -> (String, [String]) -> Int
knowledge flws ws (g, ss) = sum $ map knowledge' ss
 where
  knowledge' s = let f = mkFilter $ ws <> fromGuess s g
                     result = filter f flws
                 in  length result

-- Function which yields a known Wordle state, given the solution and a guess.
fromGuess :: String -> String -> WordleState
fromGuess solution guess =
  let wordleStates = zipWith fromGuess' (choices solution) guess
      wordleTiles = map fst wordleStates
      greyTiles = fromList $ mapMaybe snd wordleStates
  in  WordleState wordleTiles greyTiles
 where
  fromGuess' :: (Char, [Char]) -> Char -> (WordleTile, Maybe Char)
  fromGuess' (sc, oscs) gc
    | sc == gc       = ((empty,        Just gc), Nothing)
    | gc `elem` oscs = ((singleton gc, Nothing), Nothing)
    | otherwise      = ((empty,        Nothing), Just gc)

-- Helper function, given a list, returns a list of pairs, comprising an element
-- in the list and a list of all the other elements in the list.
choices :: [a] -> [(a, [a])]
choices [] = []
choices (c:rest) = (c, rest) : fmap (fmap (c:)) (choices rest)

-- Parser for a Wordle tile state argument. For example: '[]' is empty; '[ABC]'
-- is yellow tiles 'A', 'B' and 'C'; 'D' is green tile 'D'; and 'D[ABC]' is
-- green tile 'D' and yellow tiles 'A', 'B' and 'C'.
readWordleTile :: ReadP WordleTile
readWordleTile = do
  mGreenTile <- readMGreenTile <++ pure Nothing
  yellowTiles <- readYellowTiles <++ pure []
  pure (fromList yellowTiles, mGreenTile)
 where
  readMGreenTile = Just <$> satisfy isUpper
  readYellowTiles = between (char '[') (char ']') $ many (satisfy isUpper)
