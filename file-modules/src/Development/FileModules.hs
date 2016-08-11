{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Development.FileModules where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forM)
import           Data.String.Utils        (split)
import           Language.Haskell.Exts    (ImportDecl (..),
                                           ModuleHeadAndImports (..),
                                           ModuleName (..), NonGreedy (..),
                                           ParseResult (..),
#ifdef MIN_VERSION_haskell_src_exts
#  if MIN_VERSION_haskell_src_exts(1,18,0)
                                           SrcSpanInfo,
#  endif
#else
                                           SrcSpanInfo,
#endif
                                           SrcLoc (..), parse)
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

#ifdef MIN_VERSION_haskell_src_exts
#  if MIN_VERSION_haskell_src_exts(1,18,0)
getImportsFromHead :: NonGreedy (ModuleHeadAndImports SrcSpanInfo) -> [String]
getImportsFromHead (NonGreedy (ModuleHeadAndImports _ _ _ mimports)) =
    map (helper . importModule) mimports
  where
    helper (ModuleName _ iname) = iname
#  else
{-# DEPRECATED getImportsFromHead "haskell-src-exts<1.18.0 will stoped being supported in file-modules" #-}
getImportsFromHead (NonGreedy{..}) =
    map (helper . importModule) mimports
  where
    (ModuleHeadAndImports _ _ mimports) = unNonGreedy
    helper (ModuleName iname) = iname
#  endif
#else
{-# WARNING getImportsFromHead "Cabal macro to detect haskell-src-exts version not defined, assuming haskell-src-exts>1.18.0" #-}
getImportsFromHead :: NonGreedy (ModuleHeadAndImports SrcSpanInfo) -> [String]
getImportsFromHead (NonGreedy (ModuleHeadAndImports _ _ _ mimports)) =
    map (helper . importModule) mimports
  where
    helper (ModuleName _ iname) = iname
#endif

fileModules :: FilePath -> IO [String]
fileModules fname = do
    fcontents <- readFile fname
    case parse $ sanitize fcontents of
        (ParseOk rheadAndImports) -> return (getImportsFromHead rheadAndImports)
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
