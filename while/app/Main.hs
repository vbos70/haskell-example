module Main where

import Lib
import System.Console.Haskeline

main :: IO ()
main = do
  runInputT defaultSettings $ do
    line <- getInputLine "Hello: "
    case line of
      Just s -> outputStrLn $ (show (parseString s))
      _      -> outputStrLn "?!"
