{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- V1 and V2 scripts
module Untyped137 (
  writeUntyped137ScriptV1
  , writeUntyped137ScriptV2
) where

import           Cardano.Api              (writeFileTextEnvelope)
import           Cardano.Api.Shelley      (PlutusScript (PlutusScriptSerialised),
                                           PlutusScriptV1, PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Data.Functor             (void)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx                 as PTX
import qualified PlutusTx.Builtins        as BI
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless,
                                                (.))
import           Prelude                  (IO, (.))


rdmrVal :: BuiltinData
rdmrVal = BI.mkI (137 :: Integer)

{-# INLINEABLE untypedFn #-}
untypedFn :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedFn _ rdmr _ 
  | rdmr == rdmrVal = ()
  | otherwise = traceError "USING WRONG REDEEMER"


untyped137Validator :: Plutus.Validator
untyped137Validator = Plutus.mkValidatorScript $$(PTX.compile [|| untypedFn ||])

untyped137Script :: Plutus.Script
untyped137Script = Plutus.unValidatorScript untyped137Validator

untyped137SBS :: SBS.ShortByteString
untyped137SBS = SBS.toShort . LBS.toStrict $ serialise untyped137Script


untyped137SerialisedV1 :: PlutusScript PlutusScriptV1
untyped137SerialisedV1 = PlutusScriptSerialised untyped137SBS

untyped137SerialisedV2 :: PlutusScript PlutusScriptV2
untyped137SerialisedV2 = PlutusScriptSerialised untyped137SBS

writeUntyped137ScriptV1 :: IO ()
writeUntyped137ScriptV1 = void $ writeFileTextEnvelope "untyped137-v1.plutus" Nothing untyped137SerialisedV1

writeUntyped137ScriptV2 :: IO ()
writeUntyped137ScriptV2 = void $ writeFileTextEnvelope "untyped137-v2.plutus" Nothing untyped137SerialisedV2
