{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens       ((&), (.~), (^..))
import           Data.Aeson.Lens    (key, values, _Integral)
import           Network.Wreq
import           System.Environment (getArgs)

getLatestRevision :: String -> IO Integer
getLatestRevision package =
  maximum . (^.. responseBody . values . key "number" . _Integral) <$> getWith opt uri
  where opt = defaults & header "Accept" .~ ["application/json"]
        uri = "http://hackage.haskell.org/package/" ++ package ++ "/revisions/"

main :: IO ()
main = getArgs >>= getLatestRevision . head >>= print
