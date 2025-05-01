module Main where

import System.Environment (getArgs)
import MainTest (mainWith)
import Parser (parseStdin)
import API (runServer)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server", port] -> runServer (read port)
    ["server"] -> runServer 3001  -- Default port
    _ -> mainWith getArgs parseStdin