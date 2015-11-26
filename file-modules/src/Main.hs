module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM_)
import           Data.List.Utils          (uniq)
import           Development.FileModules
import           System.Environment       (getArgs)

main :: IO ()
main = do
    args <- getArgs
    modules <- mapConcurrently fileModulesRecur args
    forM_ (uniq (concat modules)) putStrLn
