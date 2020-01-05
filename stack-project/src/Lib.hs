{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Process (..)
      
    ) where

import Data.Text
import Data.Attoparsec.Text

data Process = Atom Text
             | Empty
             | Deadlock
             | Seq Process Process
             | Alt Process Process
             | Rep Process
             deriving (Eq, Show)



