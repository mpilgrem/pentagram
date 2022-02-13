{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Paths_pentagram (getDataFileName)
import Data.Char (isAsciiUpper)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP (ReadP, (<++), between, char, many,
  readP_to_S, satisfy)

-- From package containers
import Data.Set (Set, empty, fromList, notMember, singleton)

-- From package optparse-applicative
import Options.Applicative (Parser, ReadM, (<**>), argument, eitherReader,
  execParser, footer, fullDesc, header, help, helper, info, metavar, progDesc)

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
  ws <- execParser opts
  setBWords <- words <$> (readFile =<< getDataFileName "setb.wordlist")
  wordleWords <- words <$> (readFile =<< getDataFileName "Wordle.wordlist")
  let f = mkFilter ws
      setBWords' = filter (`notElem` wordleWords) setBWords
      result = filter f setBWords'
      ks' = map (\r -> (fst r, knowledge setBWords' ws r)) (choices result)
      ks = sortOn snd ks'
  print ks
 where
  opts = info (wordleState <**> helper)
    ( fullDesc
   <> progDesc "Help with (cheat at) Wordle"
   <> header "pentagram - help with (cheat at) Wordle"
   <> footer footerMsg )
  footerMsg = "Examples of valid tile states are: A, [], [BCD], A[BCD], " <>
              "where A is a green letter and BCD are yellow letters."

wordleState :: Parser WordleState
wordleState = do
  wordleTiles <- traverse wordleTileArg [1 .. 5]
  greyTiles <- greyTilesArg
  pure $ WordleState wordleTiles greyTiles

wordleTileArg :: Int -> Parser WordleTile
wordleTileArg n = argument wordleTileReader (metavar $ "TILESTATE" <> show n)

wordleTileReader :: ReadM WordleTile
wordleTileReader = eitherReader $ \s ->
  let errMsg = Left $ "Could not interpret argument '" <> s <>
                      "' as a Wordle tile state."
  in  case readP_to_S readWordleTile s of
        [] -> errMsg
        [r] -> case r of
                 (wordleTile, "") -> Right wordleTile
                 _ -> errMsg
        _ -> errMsg

greyTilesArg :: Parser (Set Char)
greyTilesArg = argument greyTilesReader (metavar "GREYTILES" <> help helpMsg)
 where
  helpMsg = "A sequence of greyed-out letters, for example: EFG. Characters " <>
            "other than A to Z are ignored. Use '.' if all letters are valid."

greyTilesReader :: ReadM (Set Char)
greyTilesReader = eitherReader $ \s ->
  Right $ fromList (filter isAsciiUpper s)

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
  readMGreenTile = Just <$> satisfy isAsciiUpper
  readYellowTiles = between (char '[') (char ']') $ many (satisfy isAsciiUpper)
