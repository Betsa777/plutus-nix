{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AMM where

-- import Plutus.V1.Ledger.Api ()

import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value
  ( CurrencySymbol (..),
    TokenName (..),
    adaSymbol,
    adaToken,
    flattenValue,
    symbols,
    valueOf,
  )
import Plutus.V2.Ledger.Api
  ( Credential (..),
    PubKeyHash,
    ScriptContext (..),
    addressCredential,
    scriptContextTxInfo,
    txInfoOutputs,
    txOutAddress,
    txOutValue,
  )
import Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.Prelude as P
import Utilities (writeValidatorToFile)
import Prelude (IO, Show)

newtype TokenA = TokenA BuiltinByteString deriving stock (Show)

PlutusTx.makeIsDataIndexed ''TokenA [('TokenA, 0)]
PlutusTx.makeLift ''TokenA

newtype TokenB = TokenB BuiltinByteString deriving stock (Show)

PlutusTx.makeIsDataIndexed ''TokenB [('TokenB, 0)]
PlutusTx.makeLift ''TokenB

type LPPKH = PubKeyHash

type Amount = Integer
type Reserve = Integer
type LPTokenNumber = Integer

type Token1Reserve = Integer

type Token2Reserve = Integer

type LPTokenCurrencySymbol = CurrencySymbol

type LPTokenName = TokenName

data AMMDatum = AMMDatum {
    reserve :: [(CurrencySymbol,Reserve)],
    lpTokNumber :: Integer
}
deriving stock (Show, Generic)
PlutusTx.makeIsDataIndexed ''AMMDatum


data AMMRedeemer
  = AddLiquidity Amount Amount LPPKH
  | Swap TokenA TokenB Amount LPPKH
  | RemoveLiquidity TokenA TokenB Amount LPPKH
  deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed
  ''AMMRedeemer
  [ ('AddLiquidity, 0),
    ('Swap, 1),
    ('RemoveLiquidity, 2)
  ]

{-# INLINEABLE ammValidator #-}
ammValidator :: AMMDatum -> AMMRedeemer -> ScriptContext -> Bool
ammValidator d r ctx =
  case r of
    AddLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH ->
      P.traceIfFalse "AddLiquidty fails" (addLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ctx)

-- Swap tA tB amount owner ->
--   P.traceIfFalse "Swap fails" (swap tA tB amount owner ctx)
-- RemoveLiquidity tA tB amount owner ->
--   P.traceIfFalse "Removal fails" (removeLiquidity tA tB amount owner ctx)

-- ajout de liquidité

{-# INLINEABLE addLiquidity #-}
addLiquidity :: Amount -> Amount -> LPPKH -> ScriptContext -> Bool
addLiquidity tok1Amount tok2Amount lpPKH ctx
  | tok1Amount <= 0 || tok2Amount <= 0 = False
  | otherwise =
         let inputs = txInfoInputs $ scriptContextTxInfo ctx
             lpInputs =
             --Je recupere l'input du script
             scriptInput = P.find (\inp -> case (addressCredential $ txOutAddress $ txInInfoResolved inp) of
                            ScriptCredential $ ValidatorHash v -> True
                                                            _  -> False) inputs
             value = txOutValue $ txInInfoResolved scriptInput
             --Verifier que l
             result = case (P.length value) P.=/ 2 of
                 True -> False
                 False -> let (c1,t1,am1) = head value
                              (c2,t2,am2) = value !! 1
                            in (am1 P.== tok1Amount P.&& am2 P.== tok2Amount)
                                               P.||
                               (am1 P.== tok2Amount P.&& am2 P.== tok1Amount)


             datum = txOutDatum $ txInInfoResolved scriptInput
             case Plutus.unsafeFromBuiltinData datum of
                OutputDatum d -> if
             -- Je recupère le hash du script
             scriptHash = addressCredential $ txOutAddress $ txInInfoResolved scriptInput



           txOutDatum of
        then
          let tokensForLp = P.sqrt tok1Amount tok2Amount
           in finalizeAdding tok1Amount tok2Amount tok1R tok2R tokensForLp lpPKH ctx
        else
          let tokensForLp =
                P.min
                  (P.divide (tok1Amount P.* lpTokNumb) tok1R)
                  (P.divide (tok2Amount P.* lpTokNumb) tok2R)
           in finalizeAdding tok1Amount tok2Amount tok1R tok2R tokensForLp lpPKH ctx

{-# INLINEABLE finalizeAdding #-}
finalizeAdding :: Amount -> Amount -> Token1Reserve -> Token2Reserve -> LPTokenNumber -> LPPKH -> ScriptContext -> Bool
finalizeAdding tok1Amount tok2Amount tok1R tok2R tokensForLp lpPKH ctx =
  let inputs = txInfoInputs (scriptContextTxInfo ctx)
      goodTok1Input = P.find (\inp ->
          P.any (\(cs,tn,am) -> am == tok1Amount)  (flattenValue $ txOutValue (txInInfoResolved  inp))
                       ) inputs
  let txOuts = getContinuingOutputs ctx
      -- scriptOuput1 correspond à l'output verifiant que le script a vraiment
      -- recu adaAmount
      scriptOuput1 = P.any (\o -> ((valueOf (txOutValue o) adaSymbol adaToken) == adaAmount)) txOuts
      -- flattenValue:: Value -> [(CurrencySymbol,TokenName,Integer)]
      -- txOutValue champ contenant la Value dans TxOut
      -- scriptOuput2 correspond à l'output verifiant que le script a vraiment
      -- recu tokenAmount
      scriptOuput2 =
        P.any
          ( \o ->
              P.any
                ( \(currSymb, tokN, amount) ->
                    (currSymb P./= adaSymbol) P.&& (amount == tokenAmount)
                )
                (flattenValue (txOutValue o))
          )
          txOuts
      -- ownerOuput correspond à l'output correspondant
      -- a celui dans lequel le LP a recu les LP tokens ou tokensForLp
      ownerOuput =
        P.any
          ( \o ->
              ((valueOf (txOutValue o) lpCurrSymb lpTokName) == tokensForLp)
                P.&& ( case addressCredential (txOutAddress o) of
                         PubKeyCredential ownerPKH -> ownerPKH == lpPKH
                         _ -> False
                     )
          )
          (txInfoOutputs (scriptContextTxInfo ctx))
   in (scriptOuput1 P.&& scriptOuput2 P.&& ownerOuput)

ammUntypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
ammUntypedValidator _ redeemer ctx =
  -- check retourne ()
  P.check
    ( ammValidator
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

-- ammValidatorScript ::
--     AuctionParams ->
--     CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
--   auctionValidatorScript params =
--     $$(PlutusTx.compile [||auctionUntypedValidator||])
--       `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||ammUntypedValidator||])

cip57 :: IO ()
cip57 = writeValidatorToFile "amm.plutus" validator
