module Main where

import Idris.Core.TT
import Idris.Main

main :: IO ()
main =
  do
    x <- return RType
    putStrLn $ "Hello, Haskell!" ++ (show x)
    runMain $ idrisMain []
    
