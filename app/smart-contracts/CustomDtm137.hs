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
module CustomDtm137 (
  writeCstmDtm137ScriptV1,
  writeCstmDtm137ScriptV2
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
import Plutus.V1.Ledger.Scripts as PLVS
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUV1
import           Prelude                  (IO, (.))

-- !!! BROKEN AND REMOVED FROM CABAL PROJECT ATM !! --
-- V2 Changed how you wrap validators and I need to dig into this --
-- WRAPPING HOW I WOULD ON V1 DOESNT WORK WITH V2. BROKEN RIGHT NOW

dtmVal :: Integer
dtmVal = 137

newtype DtmType = DtmType Integer
PTX.unstableMakeIsData ''DtmType

data ITyped
instance LTS.ValidatorTypes ITyped where
  type instance DatumType ITyped    = ()
  type instance RedeemerType ITyped = DtmType

{-# INLINEABLE typedFn #-}
typedFn :: DtmType -> () -> ScriptContext -> Bool
typedFn (DtmType dtm) _ _ = traceIfFalse "USING WRONG DATUM" (dtm == dtmVal)

dtm137Validator :: LTS.TypedValidator ITyped
dtm137Validator = LTS.mkTypedValidator @ITyped
    $$(PTX.compile [|| typedFn ||])
    $$(PTX.compile [||  _wrap ||])
  where 
    _wrap = mkUntypedValidator  @DtmType @()


dtm137ValidatorDone :: Plutus.Validator
dtm137ValidatorDone = validatorScript dtm137Validator

dtm137Script :: Plutus.Script
dtm137Script = Plutus.unValidatorScript dtm137ValidatorDone

dtm137SBS :: SBS.ShortByteString
dtm137SBS = SBS.toShort . LBS.toStrict $ serialise dtm137Script

dtm137SerialisedV1 :: PlutusScript PlutusScriptV1
dtm137SerialisedV1 = PlutusScriptSerialised dtm137SBS

dtm137SerialisedV2 :: PlutusScript PlutusScriptV2
dtm137SerialisedV2 = PlutusScriptSerialised dtm137SBS

writeCstmDtm137ScriptV1 :: IO ()
writeCstmDtm137ScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/customDtm137-v1.plutus" Nothing dtm137SerialisedV1

writeCstmDtm137ScriptV2 :: IO ()
writeCstmDtm137ScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/customDtm137-v2.plutus" Nothing dtm137SerialisedV2
