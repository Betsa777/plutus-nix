{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dep where

import Plutus.V1.Ledger.Value
  ( CurrencySymbol (..),
    TokenName (..),
    Value (..),
    adaSymbol,
    adaToken,
    flattenValue,
    valueOf,
  )
import Plutus.V1.Ledger.Interval(before)
import Plutus.V2.Ledger.Api
  ( Credential (..),
    Datum (..),
    PubKeyHash,
    ScriptContext (..),
    addressCredential,
    scriptContextTxInfo,
    txInfoOutputs,
    txOutAddress,
    txOutValue,
    POSIXTime(..)
  )
import Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Contexts (getContinuingOutputs,findDatum,txSignedBy,pubKeyOutputsAt)
import PlutusTx
import PlutusTx.Prelude as P
import PlutusTx.Sqrt (Sqrt (..), isqrt)
import Utilities (writeValidatorToFile)
import Prelude (IO)

data DDatum = DDatum{
    amount:: Integer,
    signer:: PubKeyHash,
    unlockTime:: POSIXTime
}

PlutusTx.makeIsDataIndexed ''DDatum [('DDatum, 0)]
PlutusTx.makeLift ''DDatum

data WRedeemer = WRedeemer PubKeyHash
PlutusTx.makeIsDataIndexed ''WRedeemer [('WRedeemer, 0)]
PlutusTx.makeLift ''WRedeemer


{-# INLINABLE validator #-}
validator:: DDatum -> WRedeemer -> ScriptContext -> Bool 
validator datum redeemer ctx = 
    case redeemer of 
        WRedeemer withdrawerPKH-> let txSign = txSignedBy  (scriptContextTxInfo ctx) withdrawerPKH
                                      txOutputs = txInfoOutputs (scriptContextTxInfo ctx)
                                      scriptOuput = length (getContinuingOutputs ctx) == 0
                                      timeValid = before (unlockTime datum) (txInfoValidRange $ scriptContextTxInfo ctx)  
                                      goodFunds = any (\o -> valueOf (txOutValue o) adaSymbol adaToken >= amount datum
                                                             && case addressCredential (txOutAddress o) of 
                                                                 PubKeyCredential pkh -> pkh == signer datum
                                                                 _ -> False
                                                     ) txOutputs
                                   in txSign && goodFunds && timeValid && scriptOuput
                                     
        _ -> False


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
getCbor = writeValidatorToFile "./assets/dep.plutus" validatorScript 


