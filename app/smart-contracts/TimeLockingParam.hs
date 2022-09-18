{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- added above for makeLift

-- V1 and V2 scripts
module TimeLockingParam (
  writeTimeLockingParamScriptV1,
  writeTimeLockingParamScriptV2
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

import Ledger.Address as LA

import Ledger (Address, ValidatorHash, PubKeyHash)

import Plutus.V1.Ledger.Time
import Plutus.V1.Ledger.Interval (contains, from)

import qualified Ledger.Typed.Scripts             as LTS
import Ledger.Typed.Scripts.Validators as LTS.Validators

import Plutus.V1.Ledger.Contexts (
    ScriptContext, TxInfo, TxOut, txOutAddress,
    txInfoValidRange, scriptContextTxInfo, 
    txSignedBy)
import Plutus.V1.Ledger.Scripts as PLVS
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSUV1
import           Prelude                  (IO, (.), Show)

-- changed from paymentPubKeyHash
data TimeLockingParam = TimeLockingParam {
  beneficiaryOfFunds :: PaymentPubKeyHash,
  whenAvailable :: POSIXTime
} deriving Show

-- makePubKey :: Maybe LA.PaymentPubKeyHash
-- makePubKey = toPubKeyHash $ "dc0532b5c85013cace5b61243659dd8fae39169d6dafbb2756efd86fb885ef3c"

mockData :: TimeLockingParam
mockData = TimeLockingParam {
  -- hard coded pub key and time to check against
  -- add in my test wallet pub key and test
  beneficiaryOfFunds = PaymentPubKeyHash $ "3f7846896a48c59359746ff096d63606ceb82e65900d20a9fd2b8a93",
  whenAvailable = 1660802437
  -- 8/18/22
}
--  where
--    mockTime :: POSIXTime
--    mockTime = 1596059095000 
-- PARAMETERIZING INSTEAD OF USING THE DATUM
PTX.makeLift ''TimeLockingParam
-- leaving the makeData as it show the diff from liftCode when parameterizing
-- PTX.unstableMakeIsData ''TimeLockingDatum (PARAMTERIZING INSTEAD OF USING THE DATUM)

{-# INLINEABLE timeLockingValFn #-}
timeLockingValFn :: TimeLockingParam -> () -> () -> ScriptContext -> Bool
timeLockingValFn prm () () ctx = 
   traceIfFalse "beneficiary's signature is missing" signedByBeneficiary &&
   traceIfFalse "funds still not unlocked - wait until date for unlocking has passed" hasEnoughTimePassed
  where
    _info :: TxInfo
    _info = scriptContextTxInfo ctx

    -- changed from PaymentPubKeyHash and then pass the PubKeyHash to it to convert here to check
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy _info  $ unPaymentPubKeyHash $ beneficiaryOfFunds prm
    -- signedByBeneficiary = txSignedBy _info  $ unPaymentPubKeyHash $ PaymentPubKeyHash $ beneficiaryOfFunds prm


    hasEnoughTimePassed :: Bool
    hasEnoughTimePassed = contains (from $ whenAvailable prm) $ txInfoValidRange _info

    -- NOT USING AS THE ABOVE METHOD IS BETTER - this loops all txOuts then asks (does this match the hard coded pubKey?)
    -- workaround by scanning txOuts and checking if any match
    -- https://cardano.stackexchange.com/questions/6143/can-you-manually-list-an-address-in-a-smart-contract/6157#6157
    -- outputToCorrectAddr :: TxOut -> Bool
    -- outputToCorrectAddr txo = case toPubKeyHash $ txOutAddress txo of
    --    Just pkh -> beneficiaryOfFunds mockData ==  pkh
    --    Nothing  -> False


data TimeLocking
instance LTS.ValidatorTypes TimeLocking where
  type instance DatumType TimeLocking = ()
  type instance RedeemerType TimeLocking = ()

timeLockingValidator :: TimeLockingParam -> LTS.TypedValidator TimeLocking
timeLockingValidator prm = LTS.mkTypedValidator @TimeLocking
    ($$(PTX.compile [|| timeLockingValFn ||]) `PTX.applyCode` PTX.liftCode prm)
    $$(PTX.compile [||  _wrap ||])
  where
    _wrap = mkUntypedValidator  @() @() 

timeLockingValDone ::  TimeLockingParam -> Plutus.Validator
timeLockingValDone = validatorScript . timeLockingValidator


-- have to pass in TimeLockingParam to get this to compile on V2
timeLockingScript :: TimeLockingParam -> Plutus.Script
timeLockingScript = Plutus.unValidatorScript . timeLockingValDone

-- Could add a validatorHash to use
-- Could add a script address to use
-- And so forth (endpoints and such)

valHash :: TimeLockingParam -> Ledger.ValidatorHash
valHash = validatorHash . timeLockingValidator


-- DOUBLE CHECK THIS AS I changed scriptAdress to scriptHashAdrdress then passed in the script hash
scriptAddr :: TimeLockingParam -> Ledger.Address
scriptAddr = scriptHashAddress . valHash


-- PlutusV2 issue serializing with it being parameterized
timeLockingSBS :: SBS.ShortByteString
timeLockingSBS = SBS.toShort . LBS.toStrict $ serialise $ timeLockingScript mockData

timeLockingSerialisedV1 :: PlutusScript PlutusScriptV1
timeLockingSerialisedV1 = PlutusScriptSerialised timeLockingSBS

timeLockingSerialisedV2 :: PlutusScript PlutusScriptV2
timeLockingSerialisedV2 = PlutusScriptSerialised timeLockingSBS

writeTimeLockingParamScriptV1 :: IO ()
writeTimeLockingParamScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-param-v1.plutus" Nothing timeLockingSerialisedV1

writeTimeLockingParamScriptV2 :: IO ()
writeTimeLockingParamScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/time-locking-param-v2.plutus" Nothing timeLockingSerialisedV2
