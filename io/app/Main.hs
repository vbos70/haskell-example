module Main where

-- Echo back the given string, prefixed by "ECHO: "
echoLine :: String -> String
echoLine s = "ECHO: " ++ s

-- Prefix each line in the input string by calling echoLine
echoLines :: String -> String
echoLines = unlines . (map echoLine) . lines

-- Lazily process lines from the standard input and write
-- processed lines to standard output.
main :: IO ()
main = interact echoLines
