module Main where

import HelloDatum
import AlwatsSucceeds

main :: IO ()
main = do
  putStrLn "\nSTARTING: writing plutus scripts...\n"
  writeAlwaysSucceedsScriptV2
  writeHelloDatumScriptV2
  putStrLn "\FINISHED: writing plutus scripts...\n"
