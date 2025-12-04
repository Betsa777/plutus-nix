{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DatRed where

import Plutus.V2.Ledger.Api
  ( ScriptContext (..),
      PubKeyHash(..)
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude as P
import Utilities (writeValidatorToFile)
import Prelude (IO, Show)

data DemoDatum = DemoDatum{
    signer:: PubKeyHash,
    datumValue:: Integer
}

PlutusTx.makeIsDataIndexed ''DemoDatum [('DemoDatum, 0)]
PlutusTx.makeLift ''DemoDatum

data DemoRedeemer = VerifySign | Compare Integer
PlutusTx.makeIsDataIndexed ''DemoRedeemer [
                              ('VerifySign, 0),
                              ('Compare,1)]
PlutusTx.makeLift ''DemoRedeemer

{-# INLINEABLE validator #-}
validator :: DemoDatum -> DemoRedeemer -> ScriptContext -> Bool
validator datum redeemer ctx =
     case redeemer of
      VerifySign -> txSignedBy (scriptContextTxInfo ctx) (signer datum) --verifiy if the it's the good signer
      Compare value -> value == (datumValue datum) --compare datum and redeemer value


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

getCbor :: IO ()
getCbor = writeValidatorToFile "datRed.plutus" validatorScript
