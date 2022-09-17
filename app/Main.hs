module Main where

import HelloDatum
import AlwaysSucceeds
import Untyped137
import Typed137

import CustomDtm137
import CustomRdmr137

import TimeLockingBasic


main :: IO ()
main = do
  putStrLn "\nSTARTING: writing plutus scripts...\n"
  writeAlwaysSucceedsScriptV2
  writeHelloDatumScriptV2
  writeUntyped137ScriptV2
  writeTyped137ScriptV2

  writeCstmDtm137ScriptV2
  writeCstmRdmr137ScriptV2

  writeTimeLockingBasicScriptV2
  putStrLn "\nFINISHED: writing plutus scripts...\n"
