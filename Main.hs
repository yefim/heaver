-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, DeriveDataTypeable, OverloadedStrings #-}

module Main where
import Prelude
import System.Environment
import Data.List.Split
import Text.Pandoc
import Data.Yaml.YamlLight
import Data.ByteString.Char8 (pack, unpack)
-- import Data.Maybe
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Lazy.Char8 as LZC
import Data.Data 
import Text.Hastache
import Text.Hastache.Context

data Slideshow = Slideshow {
  slides :: [String],
  title :: String,
  controls :: Bool
} deriving (Data, Typeable)

data Author = Author {
  name :: String,
  twitter :: String,
  url :: String
} deriving (Data, Typeable)

markdownToHtml :: String -> String
markdownToHtml = writeHtmlString def . readMarkdown def

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

authorSlide :: Maybe YamlLight -> IO LZC.ByteString
authorSlide Nothing  = return LZC.empty
authorSlide (Just a) = do
  let context = mkGenericContext $ Author (lookupYLString "name" a) (lookupYLString "twitter" a) (lookupYLString "url" a)
  hastacheFile defaultConfig "templates/author.mustache" context

stringToList :: LZC.ByteString -> [String]
stringToList s | LZC.null s = []
               | otherwise  = [LZC.unpack s]

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (y:s) = splitOn "\n--\n" contents
  metadata <- parseYaml y

  a <- authorSlide $ lookupYL (YStr $ pack "author") metadata
  let html = map markdownToHtml s ++ stringToList a

  let t = lookupYLString "title" metadata
  let c = lookupYLBool "controls" metadata
  let f = lookupYLString "output" metadata

  let context = mkGenericContext $ Slideshow html t c
  o <- hastacheFile defaultConfig "templates/layout.mustache" context
  LZ.writeFile f o

  putStrLn "Heaver is done."
