module Main where

import           StackRunAuto
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    let (extras, args') = getExtras args

    case args' of
        [] -> error "Usage: stack-run-auto <file>"
        ("--help":_) -> hPutStrLn stderr usage
        (fname:_) -> run (Options fname extras)
  where
    usage = unlines [ "Usage: stack-run-auto [--extra <pkg>...] <file>"
                    , ""
                    , "    --extra <pkg>   Adds an extra package that couldn't be resolved"
                    , ""
                    ]

getExtras :: [String] -> ([String], [String])
getExtras args = go ([], args)
  where
    go (extras, "--extra":pkg:args') = go (pkg:extras, args')
    go i = i
