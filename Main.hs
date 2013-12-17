-- Advanced Programming
-- by Lewis Ellis ellis and Geoffrey Vedernikoff veg

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleInstances #-}

module Main where
import Prelude
import System.Environment

main :: IO ()
main = do
  [file] <- getArgs  
  putStrLn file
