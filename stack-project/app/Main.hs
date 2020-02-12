{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Lib as L

main :: IO ()
main = do
  userInput <- getContents
  putStrLn userInput
  putStrLn (L.processToStr (L.parseProcess userInput))
