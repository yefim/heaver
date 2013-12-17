-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Main where
import Prelude
import System.Environment
import Data.List.Split
import Text.Pandoc

markdownToHtml :: String -> String
markdownToHtml = writeHtmlString def . readMarkdown def

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  -- the first "slide" is always metadata in YAML
  let (metadata:slides) = splitOn "\n--\n" contents
  let html = map markdownToHtml slides
  _ <- mapM (\s -> putStrLn $ "line: " ++ s) html -- prints for debugging
  -- writeFile outputFilename html
  putStrLn "Heaver is done."
