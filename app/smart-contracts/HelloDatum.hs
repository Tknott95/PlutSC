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
module HelloDatum where

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


-- SHOWING BOTH METHODS (USING HEX OR BYTESTRING)
helloByteString :: BuiltinData
helloByteString = BI.mkB ("Hello" :: BuiltinByteString)

-- hex of Hello (PascalCase)
-- returns a (hex -> int) conversion 
helloHex :: BuiltinData
helloHex = PTX.toBuiltinData (0x48656c6c6f21 :: Integer)

{-# INLINEABLE helloFn #-}
helloFn :: BuiltinData -> BuiltinData -> BuiltinData -> ()
helloFn dtm _ _ = if dtm P.== helloHex then () else (P.error ())

helloDatumValidator :: Plutus.Validator
helloDatumValidator = Plutus.mkValidatorScript $$(PTX.compile [|| helloFn ||])

helloDatumScript :: Plutus.Script
helloDatumScript = Plutus.unValidatorScript helloDatumValidator

helloDatumSBS :: SBS.ShortByteString
helloDatumSBS = SBS.toShort . LBS.toStrict $ serialise helloDatumScript


helloDatumSerialisedV1 :: PlutusScript PlutusScriptV1
helloDatumSerialisedV1 = PlutusScriptSerialised helloDatumSBS

helloDatumSerialisedV2 :: PlutusScript PlutusScriptV2
helloDatumSerialisedV2 = PlutusScriptSerialised helloDatumSBS

writeHelloDatumScriptV1 :: IO ()
writeHelloDatumScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/hellodatum-v1.plutus" Nothing helloDatumSerialisedV1

writeHelloDatumScriptV2 :: IO ()
writeHelloDatumScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/hellodatum-v2.plutus" Nothing helloDatumSerialisedV2
