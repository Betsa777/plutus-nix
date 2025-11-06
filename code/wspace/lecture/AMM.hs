{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AMM where

-- import Plutus.V1.Ledger.Api ()
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
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude as P
import           Utilities            (writeValidatorToFile)
import           Prelude              (IO)

newtype TokenA = TokenA BuiltinByteString

PlutusTx.makeIsDataIndexed ''TokenA [('TokenA, 0)]
PlutusTx.makeLift ''TokenA

newtype TokenB = TokenB BuiltinByteString

PlutusTx.makeIsDataIndexed ''TokenB [('TokenB, 0)]
PlutusTx.makeLift ''TokenB

type LPPKH = PubKeyHash

type Amount = Integer

type LPTokenNumber = Integer

type AdaReserve = Integer

type TokenReserve = Integer

type LPTokenCurrencySymbol = CurrencySymbol

type LPTokenName = TokenName

data AMMRedeemer
  = AddLiquidity Amount Amount AdaReserve TokenReserve LPTokenNumber LPPKH LPTokenCurrencySymbol LPTokenName
  | Swap TokenA TokenB Amount LPPKH
  | RemoveLiquidity TokenA TokenB Amount LPPKH

PlutusTx.makeIsDataIndexed
  ''AMMRedeemer
  [ ('AddLiquidity, 0),
    ('Swap, 1),
    ('RemoveLiquidity, 2)
  ]

{-# INLINEABLE ammValidator #-}
ammValidator :: AMMRedeemer -> ScriptContext -> Bool
ammValidator r ctx =
  case r of
    AddLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ->
      P.traceIfFalse "AddLiquidty fails" (addLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ctx)

-- Swap tA tB amount owner ->
--   P.traceIfFalse "Swap fails" (swap tA tB amount owner ctx)
-- RemoveLiquidity tA tB amount owner ->
--   P.traceIfFalse "Removal fails" (removeLiquidity tA tB amount owner ctx)

-- ajout de liquidité

{-# INLINEABLE addLiquidity #-}
addLiquidity :: Amount -> Amount -> AdaReserve -> TokenReserve -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
addLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ctx =
  if adaAmount <= 0 || tokenAmount <= 0
    then False
    else
      -- au cas ou il n'ya pas de reserve du token ajouté
      if tokR == 0
        then
          let tokensForLp = adaR + adaAmount
           in finalizeAdding adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx
        else
          let tokensForLp = P.divide (lpTokNumb P.* adaAmount) adaR
           in finalizeAdding adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx

{-# INLINEABLE finalizeAdding #-}
finalizeAdding :: Amount -> Amount -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
finalizeAdding adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx =
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

ammUntypedValidator:: BuiltinData -> BuiltinData-> BuiltinData -> ()
ammUntypedValidator _ redeemer ctx =
--check retourne ()
  P.check
  (
       ammValidator
       (PlutusTx.unsafeFromBuiltinData redeemer)
       (PlutusTx.unsafeFromBuiltinData ctx)
  )
-- ammValidatorScript ::
--     AuctionParams ->
--     CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
--   auctionValidatorScript params =
--     $$(PlutusTx.compile [||auctionUntypedValidator||])
--       `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

validator::PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||ammUntypedValidator ||])

cip57:: IO ()
cip57 = writeValidatorToFile "amm.plutus" validator
