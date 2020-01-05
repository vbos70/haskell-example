{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Lib as L

main :: IO ()
main = print $ L.parseProcess "Empty "
