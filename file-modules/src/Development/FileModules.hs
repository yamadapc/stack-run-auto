{-# LANGUAGE RecordWildCards #-}
module Development.FileModules where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM)
import           Data.String.Utils        (split)
import           Language.Haskell.Exts    (ImportDecl (..),
                                           ModuleHeadAndImports (..),
                                           ModuleName (..), NonGreedy (..),
                                           ParseResult (..), parse)
import           System.Directory
import           System.FilePath

fileModulesRecur :: FilePath -> IO [String]
fileModulesRecur fname = run fname
  where
    run f = do
      modules <- fileModules f
      modules' <- flip mapConcurrently modules $ \m -> do
          let pth = takeDirectory fname </> joinPath (split "." m) ++ ".hs"
          isLocalModule <- doesFileExist pth
          if isLocalModule
              -- If we're hitting a local modules, ignore it on the
              -- output (this may not be what we want)
              then run pth
              else return [m]
      return (concat modules')

fileModules :: FilePath -> IO [String]
fileModules fname = do
    result <- parse . sanitize <$> readFile fname
    case result of
        (ParseOk (NonGreedy{..})) -> do
            let (ModuleHeadAndImports _ _ mimports) = unNonGreedy
            forM mimports $ \imp ->
                let ModuleName iname = importModule imp
                in return iname
        (ParseFailed _ _) -> error $ "Failed to parse module in " ++ fname
  where
    sanitize = unlines . map removeCpp . lines
    removeCpp ('#':_) = ""
    removeCpp l = l
