module Main where

import Data.Text (pack)
import Parser
import System.IO

main :: IO ()
main = do
  handle <- openFile "specs" ReadMode
  contents <- hGetContents handle
  -- let contents = ".{ .x = true != false; }"
  let ast = runParser (pack contents)
  putStrLn $ show ast
