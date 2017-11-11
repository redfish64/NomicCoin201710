module Main where

import Idris.Core.TT
import Idris.Main

main :: IO ()
main =
  do
    x <- return RType
    putStrLn $ "Hello, Haskell!" ++ (show x)
    runMain $ idrisMain []


-- TODO 2 figure out why false is provable for %reflection and make sure it doesnt occur
-- TODO 2 figure out why false is provable in ProveFalse1.idr and make sure it doesnt occur
--   see http://okmij.org/ftp/Haskell/impredicativity-bites.html and
--       https://github.com/idris-lang/Idris-dev/issues/3687


    

