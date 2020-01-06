{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Lib as L
import System.Console.Haskeline

main :: IO ()
main = do
  runInputT defaultSettings $ do
    line <- getInputLine ""
    case line of
      Just s -> outputStrLn $ show (L.parseProcess (pack s))
      _      -> outputStrLn $ "?!#?"
  main    
