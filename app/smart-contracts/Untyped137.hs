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
module Untyped137 where

import           Cardano.Api              (writeFileTextEnvelope)
import           Cardano.Api.Shelley      (PlutusScript (PlutusScriptSerialised),
                                           PlutusScriptV1, PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Data.Functor             (void)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins        as BI
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless,
                                                (.))
import           Prelude                  (IO, (.))

rdmrVal :: BuiltinData
rdmrVal = BI.mkI 137

{-# INLINEABLE untypedFn #-}
untypedFn :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedFn _ rdmr _ 
  | rdmr == rdmrVal = ()
  | otherwise = traceError "USING WRONG REDEEMER"


untypedFnValidator :: Plutus.Validator
untypedFnValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| untypedFn ||])


untypedFnScript :: Plutus.Script
untypedFnScript = Plutus.unValidatorScript untypedFnValidator


untypedFnSBS :: SBS.ShortByteString
untypedFnSBS = SBS.toShort . LBS.toStrict $ serialise untypedFnScript


untypedFnSerialisedV1 :: PlutusScript PlutusScriptV1
untypedFnSerialisedV1 = PlutusScriptSerialised untypedFnSBS

writeUntypedFnScriptV1 :: IO ()
writeUntypedFnScriptV1 = void $ writeFileTextEnvelope "untyped137-v1.plutus" Nothing untypedFnSerialisedV1


untypedFnSerialisedV2 :: PlutusScript PlutusScriptV2
untypedFnSerialisedV2 = PlutusScriptSerialised untypedFnSBS

writeUntypedFnScriptV2 :: IO ()
writeUntypedFnScriptV2 = void $ writeFileTextEnvelope "untyped137-v2.plutus" Nothing untypedFnSerialisedV2
