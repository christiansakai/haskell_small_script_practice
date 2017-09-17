#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
-}

import Data.List          (intercalate)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= putStrLn . showArgs 

showArgs :: [String] -> String
showArgs args = intercalate " " args
