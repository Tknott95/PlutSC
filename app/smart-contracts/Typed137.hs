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
module Typed137 (
  writeTyped137ScriptV1,
  writeTyped137ScriptV2
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

import qualified Ledger.Typed.Scripts             as TSCS
-- import Ledger.Typed.Scripts.Validators as TSCS.Validators
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as PSU.V1

import           Prelude                  (IO, (.))


rdmrVal :: Integer
rdmrVal = 137

data ITyped
instance TSCS.ValidatorTypes ITyped where
  type instance DatumType ITyped    = ()
  type instance RedeemerType ITyped = Integer

{-# INLINEABLE typedFn #-}
typedFn :: () -> Integer -> BuiltinData -> ()
typedFn _ rdmr _ = traceIfFalse "USING WRONG REDEEMER" (rdmr == rdmrVal)

typed137Validator :: TSCS.TypedValidator ITyped
typed137Validator = TSCS.mkTypedValidator @ITyped
    $$(PTX.compile [|| typedFn ||])
    $$(PTX.compile [||  _wrap ||])
  where 
    wrap = PSU.V1.wrapValidator @() @Integer 

typed137Script :: Plutus.Script
typed137Script = Plutus.unValidatorScript typed137Validator

typed137SBS :: SBS.ShortByteString
typed137SBS = SBS.toShort . LBS.toStrict $ serialise typed137Script

typed137SerialisedV1 :: PlutusScript PlutusScriptV1
typed137SerialisedV1 = PlutusScriptSerialised typed137SBS

typed137SerialisedV2 :: PlutusScript PlutusScriptV2
typed137SerialisedV2 = PlutusScriptSerialised typed137SBS

writeTyped137ScriptV1 :: IO ()
writeTyped137ScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/typed137-v1.plutus" Nothing typed137SerialisedV1

writeTyped137ScriptV2 :: IO ()
writeTyped137ScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/typed137-v2.plutus" Nothing typed137SerialisedV2
