{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib as L

main :: IO ()
main = print $ L.Alt L.Empty (L.Atom "Hallo")
