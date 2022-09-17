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
module TimeLockingParam (
    writeTimeLockingParamScriptV1,
    writeTimeLockingParamScriptV2)
where

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

import Ledger.Address as LA

import Plutus.V1.Ledger.Time
import Plutus.V1.Ledger.Interval (contains, from)

import qualified Ledger.Typed.Scripts             as LTS
import Ledger.Typed.Scripts.Validators as LTS.Validators

import Plutus.V1.Ledger.Contexts (
    ScriptContext, TxInfo, 
    txInfoValidRange, scriptContextTxInfo, 
    txSignedBy)
import Plutus.V1.Ledger.Scripts as PLVS
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUV1
import           Prelude                  (IO, (.), Show)


data TimeLockingParam = TimeLockingParam {
  beneficiaryOfFunds :: LA.PaymentPubKeyHash,
  whenAvailable :: POSIXTime
} deriving Show

-- PARAMETERIZING INSTEAD OF USING THE DATUM
PTX.makeLift ''TimeLockingDatum 
-- PTX.unstableMakeIsData ''TimeLockingDatum (PARAMTERIZING INSTEAD OF USING THE DATUM)

{-# INLINEABLE timeLockingValFn #-}
timeLockingValFn :: TimeLockingParam -> () -> () -> ScriptContext -> Bool
timeLockingValFn prm _ _ ctx = 
   traceIfFalse "beneficiary's signature is missing" signedByBeneficiary &&
   traceIfFalse "funds still not unlocked - wait until date for unlocking has passed" hasEnoughTimePassed
  where
    _info :: TxInfo
    _info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy _info $ unPaymentPubKeyHash $ beneficiaryOfFunds prm

    hasEnoughTimePassed :: Bool
    hasEnoughTimePassed = contains (from $ whenAvailable prm) $ txInfoValidRange _info


data TimeLocking
instance LTS.ValidatorTypes TimeLocking where
  type instance DatumType TimeLocking = ())
  type instance RedeemerType TimeLocking = ()

timeLockingValidator :: TimeLockingParam LTS.TypedValidator TimeLocking
timeLockingValidator prm = LTS.mkTypedValidator @TimeLocking
    $$(PTX.compile [|| timeLockingValFn ||]) `PTX.applyCode` PTX.liftCode prm
    $$(PTX.compile [||  _wrap ||])
  where
    _wrap = mkUntypedValidator  @() @() 

timeLockingValDone :: Plutus.Validator
timeLockingValDone = validatorScript timeLockingValidator

timeLockingScript :: Plutus.Script
timeLockingScript = Plutus.unValidatorScript timeLockingValDone

-- Could add a validatorHash to use
-- Could add a script address to use
-- And so forth (endpoints and such)

timeLockingSBS :: SBS.ShortByteString
timeLockingSBS = SBS.toShort . LBS.toStrict $ serialise timeLockingScript

timeLockingSerialisedV1 :: PlutusScript PlutusScriptV1
timeLockingSerialisedV1 = PlutusScriptSerialised timeLockingSBS

timeLockingSerialisedV2 :: PlutusScript PlutusScriptV2
timeLockingSerialisedV2 = PlutusScriptSerialised timeLockingSBS

writeTimeLockingParamScriptV1 :: IO ()
writeTimeLockingParamScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-param-v1.plutus" Nothing timeLockingSerialisedV1

writeTimeLockingParamScriptV2 :: IO ()
writeTimeLockingParamScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-param-v2.plutus" Nothing timeLockingSerialisedV2
