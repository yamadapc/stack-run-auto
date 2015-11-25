module Main where

import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Set                        as Set
import           Development.ExtractDependencies
import           System.Environment

main :: IO ()
main = do
    npkgs <- getArgs
    deps <- mapConcurrently extractDependencies npkgs
    forM_ (uniq (concat deps)) putStrLn
  where
      uniq = uniq' Set.empty
        where
          uniq' _ [] = []
          uniq' x (y:ys) = if Set.member y x
                           then uniq' x ys
                           else y : uniq' (Set.insert y x) ys
