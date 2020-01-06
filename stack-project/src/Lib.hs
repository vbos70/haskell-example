{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Process (..),
      processParser,
      parseProcess
    ) where

import Control.Applicative
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
processParser = Empty <$ (string "Empty") <* endOfInput

parseProcess :: Text -> Process
parseProcess t = toProcess (parseOnly processParser t)
  where toProcess (Left msg) = Error (pack msg)
        toProcess (Right p) = p

