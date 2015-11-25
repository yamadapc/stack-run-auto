{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens       ((&), (.~), (^..), (^.))
import           Data.Aeson.Lens    (key, values, _Integral)
import           Network.Wreq
import           System.Environment (getArgs)
import           Data.ByteString.Lazy.Char8 (ByteString)

getLatestRevision :: String -> IO Integer
getLatestRevision package =
  maximum . (^.. responseBody . values . key "number" . _Integral) <$> getWith opt uri
  where opt = defaults & header "Accept" .~ ["application/json"]
        uri = "http://hackage.haskell.org/package/" ++ package ++ "/revisions/"

getPackageRevision :: String -> Integer -> IO ByteString
getPackageRevision package revision =
  (^. responseBody) <$> get uri
  where uri = "http://hackage.haskell.org/package/" ++ package ++ "/revision/" ++ show revision

main :: IO ()
main = do
  package <- head <$> getArgs
  getLatestRevision package >>= getPackageRevision package >>= print
