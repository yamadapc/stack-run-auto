{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad         (forM_)
import           Language.Haskell.Exts (ImportDecl (..),
                                        ModuleHeadAndImports (..),
                                        ModuleName (..), NonGreedy (..),
                                        ParseResult (..), parse)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    forM_ args fileModules

fileModules :: FilePath -> IO ()
fileModules fname = do
    result <- parse <$> readFile fname
    case result of
        (ParseOk (NonGreedy{..})) -> do
            let (ModuleHeadAndImports _ _ mimports) = unNonGreedy
            forM_ mimports $ \imp ->
                let ModuleName iname = importModule imp
                in putStrLn iname
        (ParseFailed _ _) -> exitFailure
