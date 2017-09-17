#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
-}

import Data.Foldable      (forM_)
import Data.Traversable   (mapM)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM readFile >>= putStrLn . concat
