module Main where

import Control.Monad (unless)
import JsonParser
import System.Environment.Blank (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  args <- getArgs
  unless (null args) $ do
    let filePath = head args
    result <- parseFromFile jsonValue filePath
    print result
