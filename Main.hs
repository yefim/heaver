-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Main where
import Prelude
import System.Environment
import Data.List.Split
import Text.Pandoc
import Data.Yaml.YamlLight
import Data.ByteString.Char8 (pack)
import Data.Maybe

markdownToHtml :: String -> String
markdownToHtml = writeHtmlString def . readMarkdown def

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let (yaml:slides) = splitOn "\n--\n" contents
  let html = map markdownToHtml slides
  metadata <- parseYaml yaml
  -- let author = fromJust $ lookupYL (YStr $ pack "author") metadata
  -- let name = fromJust $ lookupYL (YStr $ pack "name") author
  -- _ <- mapM (\s -> putStrLn $ "line: " ++ s) html -- prints for debugging
  -- writeFile outputFilename html
  putStrLn "Heaver is done."
