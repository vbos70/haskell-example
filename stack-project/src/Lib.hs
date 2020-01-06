{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Process (..),
      parseProcess
    ) where

import Control.Applicative
import Data.Char
import Data.Text
import Data.Attoparsec.Text

data Process = Atom Text
             | Empty
             | Deadlock
             | Seq Process Process
             | Alt Process Process
             | Rep Process
             | Error Text
             deriving (Eq, Show)

processParser :: Parser Process
processParser = simpleProcess <* endOfInput

simpleProcess :: Parser Process
simpleProcess = emptyParser <|> deadlockParser <|> atomParser

emptyParser :: Parser Process
emptyParser = Empty <$ (string "!")

deadlockParser :: Parser Process
deadlockParser = Deadlock <$ (string "?")

atomParser :: Parser Process
atomParser = Atom <$> do
  s <- many1 (letter <|> digit)
  return (pack s)

parseProcess :: Text -> Process
parseProcess t = toProcess (parseOnly processParser t)
  where toProcess (Left msg) = Error (pack msg)
        toProcess (Right p) = p

