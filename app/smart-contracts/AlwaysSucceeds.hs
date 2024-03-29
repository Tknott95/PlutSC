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
module AlwaysSucceeds (
    writeAlwaysSucceedsScriptV1,
    writeAlwaysSucceedsScriptV2,
  )
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
import           Prelude                  (IO, (.))


{-# INLINEABLE alwaysSucceeds #-}
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsValidator :: Plutus.Validator
alwaysSucceedsValidator = Plutus.mkValidatorScript $$(PTX.compile [|| alwaysSucceeds ||])

alwaysSucceedsScript :: Plutus.Script
alwaysSucceedsScript = Plutus.unValidatorScript alwaysSucceedsValidator

alwaysSucceedsSBS :: SBS.ShortByteString
alwaysSucceedsSBS = SBS.toShort . LBS.toStrict $ serialise alwaysSucceedsScript


alwaysSucceedsSerialisedV1 :: PlutusScript PlutusScriptV1
alwaysSucceedsSerialisedV1 = PlutusScriptSerialised alwaysSucceedsSBS

alwaysSucceedsSerialisedV2 :: PlutusScript PlutusScriptV2
alwaysSucceedsSerialisedV2 = PlutusScriptSerialised alwaysSucceedsSBS

writeAlwaysSucceedsScriptV1 :: IO ()
writeAlwaysSucceedsScriptV1 = void $ writeFileTextEnvelope "compiled-scripts/alwayssucceeds-v1.plutus" Nothing alwaysSucceedsSerialisedV1

writeAlwaysSucceedsScriptV2 :: IO ()
writeAlwaysSucceedsScriptV2 = void $ writeFileTextEnvelope "compiled-scripts/alwayssucceeds-v2.plutus" Nothing alwaysSucceedsSerialisedV2
