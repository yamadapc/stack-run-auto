{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Cursor            (attribute, attributeIs, element,
                                             fromDocument, ($//), (&/), (&|))

main :: IO ()
main =
  (fromDocument . parseLBS) <$> L.getContents
                            >>= (\cursor -> display $ cursor $// selector &| extract)
  where selector = element "td" &/ attributeIs "id" "detailed-dependencies" &/
                   element "ul" &/ element "li" &/ element "a"
        extract = snd . T.breakOnEnd "/package/" . T.concat . attribute "href"
        display = mapM_ (putStrLn . T.unpack)
