{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AlwaysS where

import Plutus.V2.Ledger.Api
  ( ScriptContext (..),
  )
import Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude as P
import Utilities (writeValidatorToFile)
import Prelude (IO, Show)

{-# INLINEABLE validator #-}
validator :: Integer -> Integer -> ScriptContext -> Bool
validator d r ctx =
  d == r

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator datum redeemer ctx =
  -- check retourne ()
  P.check
    ( validator
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

validatorScript :: PlutusV2.Validator
validatorScript = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||untypedValidator||])

cip57 :: IO ()
cip57 = writeValidatorToFile "alwaysS.plutus" validatorScript
