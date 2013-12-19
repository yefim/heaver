-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, DeriveDataTypeable, OverloadedStrings #-}

module Main where
import Prelude
import Test.HUnit
import System.Environment
import Data.List.Split
import Text.Pandoc
import Data.Yaml.YamlLight
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Lazy.Char8 as LZC
import Data.Data 
import Text.Hastache
import Text.Hastache.Context

data Layout = Layout {
  slideshow :: String,
  title :: String,
  encoding :: String
} deriving (Data, Typeable)

data Slideshow = Slideshow {
  slides :: [String],
  controls :: Bool,
  progress :: Bool
} deriving (Data, Typeable)

data Author = Author {
  name :: String,
  twitter :: String,
  url :: String
} deriving (Data, Typeable)

markdownToHtml :: String -> String
markdownToHtml = writeHtmlString def . readMarkdown def

t0 :: Test
t0 = TestList [markdownToHtml "# test" ~?= "<h1 id=\"test\">test</h1>",
               markdownToHtml "paragraph" ~?= "<p>paragraph</p>",
               markdownToHtml "* test" ~?= "<ul>\n<li>test</li>\n</ul>",
               markdownToHtml "" ~?= "",
               markdownToHtml "[link](example.com)" ~?= "<p><a href=\"example.com\">link</a></p>"]

lookupYLString :: String -> YamlLight -> String
lookupYLString k y = case lookupYL (YStr $ pack k) y of
                       Just v -> case unStr v of
                         Just b -> unpack b
                         _      -> ""
                       _      -> ""

lookupYLBool :: String -> YamlLight -> Bool
lookupYLBool k y = case lookupYLString k y of
                      "true" -> True
                      _      -> False

renderAuthorSlide :: Maybe YamlLight -> IO LZC.ByteString
renderAuthorSlide Nothing  = return LZC.empty
renderAuthorSlide (Just a) =
  let n = lookupYLString "name" a in
  let t = lookupYLString "twitter" a in
  let u = lookupYLString "url" a in
  let ac = mkGenericContext $ Author n t u in
  hastacheFile defaultConfig "templates/author.mustache" ac

renderSlideshow :: YamlLight -> [String] -> IO LZC.ByteString
renderSlideshow m s =
  let c = lookupYLBool "controls" m in
  let p = lookupYLBool "progress" m in
  let sc = mkGenericContext $ Slideshow s c p in
  hastacheFile defaultConfig "templates/default.mustache" sc

renderOutput :: YamlLight -> String -> IO LZC.ByteString
renderOutput m s =
  let t = lookupYLString "title" m in
  let e = lookupYLString "encoding" m in
  let oc = mkGenericContext $ Layout s t e in
  hastacheFile defaultConfig "templates/layout.mustache" oc

stringToList :: LZC.ByteString -> [String]
stringToList s | LZC.null s = []
               | otherwise  = [LZC.unpack s]

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (y:s) = splitOn "\n--\n" contents
  metadata <- parseYaml y

  a <- renderAuthorSlide $ lookupYL (YStr $ pack "author") metadata
  let html = map markdownToHtml s ++ stringToList a

  sw <- renderSlideshow metadata html

  let f = lookupYLString "output" metadata
  output <- renderOutput metadata $ LZC.unpack sw
  LZ.writeFile f output

  putStrLn "Heaver is done."
