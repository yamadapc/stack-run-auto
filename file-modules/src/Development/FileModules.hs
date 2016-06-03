{-# LANGUAGE RecordWildCards #-}
module Development.FileModules where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM)
import           Data.String.Utils        (split)
import           Language.Haskell.Exts    (ImportDecl (..),
                                           ModuleHeadAndImports (..),
                                           ModuleName (..), NonGreedy (..),
                                           ParseResult (..), SrcLoc (..), parse)
import           System.Directory
import           System.FilePath
import           Text.Regex

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
    fcontents <- readFile fname
    case parse $ sanitize fcontents  of
        (ParseOk NonGreedy{..}) -> do
            let (ModuleHeadAndImports _ _ mimports) = unNonGreedy
            forM mimports $ \imp ->
                let ModuleName iname = importModule imp
                in return iname
        (ParseFailed (SrcLoc _ line col) err) -> error $
            "Failed to parse module in " ++ fname ++ ":\n" ++
            "  (" ++ show line ++ ":" ++ show col ++ ") " ++ err ++ "\n" ++
            "  " ++ getLineCol fcontents (line, col)
  where
    sanitize =
        unlines . map (removeMagicHash . removeCpp) . lines
    removeCpp ('#':_) = ""
    removeCpp l = l
    removeMagicHash l = subRegex r l o
      where
        r = mkRegex "#"
        o = ""
    getLineCol fcontents (line, col) =
        ln ++ "\n" ++
        "  " ++ replicate (col' - 3) ' ' ++ "^^^"
      where
        ln = lines fcontents !! line
        col' = let l = length ln
               in if col > l then l else col
