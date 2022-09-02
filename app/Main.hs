module Main where

import HelloDatum
import AlwaysSucceeds

main :: IO ()
main = do
  putStrLn "\nSTARTING: writing plutus scripts...\n"
  writeAlwaysSucceedsScriptV2
  writeHelloDatumScriptV2
  putStrLn "\FINISHED: writing plutus scripts...\n"
