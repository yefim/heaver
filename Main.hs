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
-- import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Lazy.Char8 as LZC
import Text.Hastache
import Text.Hastache.Context
import Data.Data 
import OurStache
import ParserCombinators

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

lookupYLStringWithDefault :: String -> String -> YamlLight -> String
lookupYLStringWithDefault d k y = case lookupYL (YStr $ pack k) y of
                         Just v -> case unStr v of
                           Just b -> unpack b
                           _      -> d
                         _      -> d

lookupYLString :: String -> YamlLight -> String
lookupYLString = lookupYLStringWithDefault ""

lookupYLBool :: String -> YamlLight -> Bool
lookupYLBool k y = case lookupYLString k y of
                      "true" -> True
                      _      -> False

-- renderAuthorSlide :: Maybe YamlLight -> IO LZC.ByteString
renderAuthorSlide :: Maybe YamlLight -> Either ParseError Expr -> String
renderAuthorSlide (Just a) (Right e) =
  let n = lookupYLString "name" a in
  let t = lookupYLString "twitter" a in
  let u = lookupYLString "url" a in
  let m = lookupYLString "email" a in
  let ac = buildCtx [("name", Single $ StringVal n),
                     ("twitter", Single $ StringVal t),
                     ("url", Single $ StringVal u),
                     ("email", Single $ StringVal m)] in
  pp e ac
renderAuthorSlide _  _ = ""
  {-
  let ac = mkGenericContext $ Author n t u in
  hastacheFile defaultConfig "templates/author.mustache" ac
  -}

renderSlideshow :: YamlLight -> [String] -> Either ParseError Expr-> String
renderSlideshow m s (Right e) =
  let c = lookupYLBool "controls" m in
  let p = lookupYLBool "progress" m in
  let sc = buildCtx [("slides", List $ map StringVal s),
                     ("controls", Single $ BoolVal c),
                     ("progress", Single $ BoolVal p)] in
  pp e sc
  -- let sc = mkGenericContext $ Slideshow s c p in
  -- hastacheFile defaultConfig "templates/default.mustache" sc
renderSlideshow _ _ _ = ""

renderOutput :: YamlLight -> String -> IO LZC.ByteString
renderOutput m s =
  let t = lookupYLString "title" m in
  let c = lookupYLString "encoding" m in
  {-
  let oc = buildCtx [("slideshow", Single $ StringVal s),
                     ("title", Single $ StringVal t),
                     ("encoding", Single $ StringVal c)] in
  -}
  let oc = mkGenericContext $ Layout s t c in
  hastacheFile defaultConfig "templates/layout.mustache" oc
  -- pp e oc
-- renderOutput _ _ _ = ""

{-
stringToList :: LZC.ByteString -> [String]
stringToList s | LZC.null s = []
               | otherwise  = [LZC.unpack s]
-}

stringToList :: String -> [String]
stringToList "" = []
stringToList s  = [s]

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (y:s) = splitOn "\n--\n" contents
  metadata <- parseYaml y

  ap <- parseFromFile stacheP "templates/author.mustache"
  let a = renderAuthorSlide (lookupYL (YStr $ pack "author") metadata) ap
  let html = map markdownToHtml s ++ stringToList a

  swp <- parseFromFile stacheP "templates/default.mustache"
  let sw = renderSlideshow metadata html swp

  let f = lookupYLStringWithDefault (filename ++ ".heaver") "output" metadata
  -- op <- parseFromFile stacheP "templates/default.mustache"
  o <- renderOutput metadata sw
  LZC.writeFile f o

  putStrLn "Heaver is done."
