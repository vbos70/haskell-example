module Lib
    ( Process (..)
      
    ) where

data Process = Atom String
             | Empty
             | Deadlock
             | Seq Process Process
             | Alt Process Process
             | Rep Process
             deriving (Show)



