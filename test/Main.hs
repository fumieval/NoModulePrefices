module Main where

import Monad

main :: IO ()
main = when True $ forM_ "test" print
