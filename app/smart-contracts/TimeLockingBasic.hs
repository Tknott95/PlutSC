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
module TimeLockingBasic (
    writeTimeLockingBasicScriptV1,
    writeTimeLockingBasicScriptV2)
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


data TimeLockingDatum = TimeLockingDatum {
  beneficiaryOfFunds :: LA.PaymentPubKeyHash,
  whenAvailable :: POSIXTime
} deriving Show

PTX.unstableMakeIsData ''TimeLockingDatum

{-# INLINEABLE timeLockingValFn #-}
timeLockingValFn :: TimeLockingDatum -> () -> ScriptContext -> Bool
timeLockingValFn dtm _ ctx = 
   traceIfFalse "beneficiary's signature is missing" signedByBeneficiary &&
   traceIfFalse "funds still not unlocked - wait until date for unlocking has passed" hasEnoughTimePassed
  where
    _info :: TxInfo
    _info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy _info $ unPaymentPubKeyHash $ beneficiaryOfFunds dtm

    hasEnoughTimePassed :: Bool
    hasEnoughTimePassed = contains (from $ whenAvailable dtm) $ txInfoValidRange _info


data TimeLocking
instance LTS.ValidatorTypes TimeLocking where
  type instance DatumType TimeLocking = TimeLockingDatum
  type instance RedeemerType TimeLocking = ()

timeLockingValidator :: LTS.TypedValidator TimeLocking
timeLockingValidator = LTS.mkTypedValidator @TimeLocking
    $$(PTX.compile [|| timeLockingValFn ||])
    $$(PTX.compile [||  _wrap ||])
  where
    _wrap = mkUntypedValidator  @TimeLockingDatum @() 

timeLockingValDone :: Plutus.Validator
timeLockingValDone = validatorScript timeLockingValidator

timeLockingScript :: Plutus.Script
timeLockingScript = Plutus.unValidatorScript timeLockingValDone

-- Could add a validatorHash to use
-- Could add a script address to use
-- And so forth

timeLockingSBS :: SBS.ShortByteString
timeLockingSBS = SBS.toShort . LBS.toStrict $ serialise timeLockingScript


timeLockingSerialisedV1 :: PlutusScript PlutusScriptV1
timeLockingSerialisedV1 = PlutusScriptSerialised timeLockingSBS

timeLockingSerialisedV2 :: PlutusScript PlutusScriptV2
timeLockingSerialisedV2 = PlutusScriptSerialised timeLockingSBS

writeTimeLockingBasicScriptV1 :: IO ()
writeTimeLockingBasicScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-basic-v1.plutus" Nothing timeLockingSerialisedV1

writeTimeLockingBasicScriptV2 :: IO ()
writeTimeLockingBasicScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-basic-v2.plutus" Nothing timeLockingSerialisedV2
