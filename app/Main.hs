module Main where

import HelloDatum
import AlwaysSucceeds
import Untyped137

main :: IO ()
main = do
  putStrLn "\nSTARTING: writing plutus scripts...\n"
  writeAlwaysSucceedsScriptV2
  writeHelloDatumScriptV2
  writeUntyped137ScriptV2
  putStrLn "\nFINISHED: writing plutus scripts...\n"
