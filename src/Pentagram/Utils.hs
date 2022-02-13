{-# LANGUAGE FlexibleContexts #-}

module Pentagram.Utils where

import Control.Monad (foldM)
import Text.Printf (printf)

-- From package array
import Data.Array.ST
import Data.Array.Unboxed

-- Type synonym representing histographs for the frequency of letters at each
-- tile position.
type Histogram = UArray (Int, Char) Int

-- A function that yields a histogram, given a list of five-letter words.
histogram :: [String] -> Histogram
histogram ws = runSTUArray $ do
  arr <- newArray ((0, 'A'), (4, 'Z')) 0
  foldM countLetters arr ws

-- A function that updates a mutable array for the letters in a five-letter
-- word. Does not check that the word has five letters.
countLetters :: MArray (STUArray s) Int m
             => STUArray s (Int, Char) Int
             -> String
             -> m (STUArray s (Int, Char) Int)
countLetters arr w = foldM countLetter arr $ zip [0 .. 4] w

-- A function that updates a mutable array for a letter at a tile position.
countLetter :: MArray (STUArray s) Int m
            => STUArray s (Int, Char) Int
            -> (Int, Char)
            -> m (STUArray s (Int, Char) Int)
countLetter arr i = do
  n <- readArray arr i
  writeArray arr i (n + 1)
  pure arr

-- A function that prints a historgram nicely to standard output.
printHistogram :: Histogram -> IO ()
printHistogram h = mapM_ printRow ['A' .. 'Z']
 where
  printRow :: Char -> IO ()
  printRow c = do
    putStr $ c : " "
    mapM_ (printEntry c) [0 .. 4]
    printf "%3d" $ sum $ map (\i -> h ! (i, c)) [0 .. 4]
    putStr "\n"

  printEntry :: Char -> Int -> IO ()
  printEntry c i = printf "%2d " $ h ! (i, c)
