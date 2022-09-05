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
module CustomRdmr137 (
  writeCstmRdmr137ScriptV1,
  writeCstmRdmr137ScriptV2
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

import qualified Ledger.Typed.Scripts             as LTS
import Ledger.Typed.Scripts.Validators as LTS.Validators

import Plutus.V1.Ledger.Contexts (ScriptContext)

import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUV1
import           Prelude                  (IO, (.))


-- !!! BROKEN AND REMOVED FROM CABAL PROJECT ATM !! --
-- V2 Changed how you wrap validators and I need to dig into this --
-- WRAPPING HOW I WOULD ON V1 DOESNT WORK WITH V2. BROKEN RIGHT NOW

rdmrVal :: Integer
rdmrVal = 137

newtype RdmrType = RdmrType Integer
PTX.unstableMakeIsData ''RdmrType

data ITyped
instance LTS.ValidatorTypes ITyped where
  type instance DatumType ITyped    = ()
  type instance RedeemerType ITyped = RdmrType

{-# INLINEABLE typedFn #-}
typedFn :: () -> RdmrType -> ScriptContext -> Bool
typedFn _ (RdmrType rdmr) _ = traceIfFalse "USING WRONG REDEEMER" (rdmr == rdmrVal)

rdmr137Validator :: LTS.TypedValidator ITyped
rdmr137Validator = LTS.mkTypedValidator @ITyped
    $$(PTX.compile [|| typedFn ||])
    $$(PTX.compile [||  _wrap ||])
  where 
    -- wrap = PSUV1.wrapValidator @RdmrType 
    wrap = LTS.Validators.wrapValidator @() @RdmrType 
    -- best way I know for v2 atm using mkUntypedValidator

rdmr137ValidatorDone :: Plutus.Validator
rdmr137ValidatorDone = validatorScript rdmr137Validator

rdmr137Script :: Plutus.Script
rdmr137Script = Plutus.unValidatorScript rdmr137ValidatorDone

rdmr137SBS :: SBS.ShortByteString
rdmr137SBS = SBS.toShort . LBS.toStrict $ serialise rdmr137Script

rdmr137SerialisedV1 :: PlutusScript PlutusScriptV1
rdmr137SerialisedV1 = PlutusScriptSerialised rdmr137SBS

rdmr137SerialisedV2 :: PlutusScript PlutusScriptV2
rdmr137SerialisedV2 = PlutusScriptSerialised rdmr137SBS

writeCstmRdmr137ScriptV1 :: IO ()
writeCstmRdmr137ScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/customrdmr137-v1.plutus" Nothing rdmr137SerialisedV1

writeCstmRdmr137ScriptV2 :: IO ()
writeCstmRdmr137ScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/customrdmr137-v2.plutus" Nothing rdmr137SerialisedV2
