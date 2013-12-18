-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, DeriveDataTypeable #-}

module Main where
import Prelude
import System.Environment
import Data.List.Split
import Text.Pandoc
import Data.Yaml.YamlLight
-- import Data.ByteString.Char8 (pack)
-- import Data.Maybe
import qualified Data.ByteString.Lazy as LZ
import Data.Data 
import Text.Hastache
import Text.Hastache.Context

markdownToHtml :: String -> String
markdownToHtml = writeHtmlString def . readMarkdown def

template :: String
template = "{{#slides}}\n{{{ . }}}\n{{/slides}}"

data Slides = Slides { slides :: [String] } deriving (Data, Typeable)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (y:s) = splitOn "\n--\n" contents
  let html = map markdownToHtml s
  metadata <- parseYaml y

  let context = mkGenericContext $ Slides html
  o <- hastacheStr defaultConfig (encodeStr template) context
  LZ.writeFile "out.html" o

  putStrLn "Heaver is done."
