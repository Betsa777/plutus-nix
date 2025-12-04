{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AMMTT where

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

type LPTokenNumber = Integer

type TokenReserve = Integer

type TokenSymbol = CurrencySymbol

type LPTokenCurrencySymbol = CurrencySymbol

type LPTokenName = TokenName

data AMMRedeemer
  = AddLiquidity Amount Amount TokenReserve TokenReserve LPTokenNumber LPPKH LPTokenCurrencySymbol LPTokenName
  | Swap Amount Amount TokenReserve TokenReserve TokenSymbol
  | RemoveLiquidity TokenA TokenB Amount LPPKH
  deriving stock (Show, Generic)

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
    AddLiquidity amount1 amount2 tok1R tok2R lpTokNumb lpPKH lpCurrSymb lpTokName ->
      P.traceIfFalse "AddLiquidty fails" (addLiquidity amount1 amount2 tok1R tok2R lpTokNumb lpPKH lpCurrSymb lpTokName ctx)
    Swap amount minAmount adaR tokR tokSymbol ->
      P.traceIfFalse "Swap fails" (swap amount minAmount tok1R tokR tokSymbol ctx)

-- Swap tA tB amount owner ->
--   P.traceIfFalse "Swap fails" (swap tA tB amount owner ctx)
-- RemoveLiquidity tA tB amount owner ->
--   P.traceIfFalse "Removal fails" (removeLiquidity tA tB amount owner ctx)

-- ajout de liquidité

{-# INLINEABLE addLiquidity #-}
addLiquidity :: Amount -> Amount -> TokenReserve -> TokenReserve -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
addLiquidity amount1 amount2 tok1R tok2R lpTokNumb lpPKH lpCurrSymb lpTokName ctx =
  if amount1 <= 0 || amount2 <= 0
    then False
    else
      -- au cas ou il n'ya pas de reserve du token ajouté
      if tokR == 0
        then
          let tokensForLp = adaR + adaAmount
           in finalizeAdding adaR tokR adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx
        else
          let tokensForLp = P.divide (lpTokNumb P.* adaAmount) adaR
           in finalizeAdding adaR tokR adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx

{-# INLINEABLE finalizeAdding #-}
finalizeAdding :: AdaReserve -> TokenReserve -> Amount -> Amount -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
finalizeAdding adaR tokR adaAmount tokenAmount tokensForLp lpPKH lpCurrSymb lpTokName ctx =
  let txOuts = getContinuingOutputs ctx
      -- scriptOuput1 correspond à l'output verifiant que le script a vraiment
      -- recu adaAmount
      scriptOuput1 = P.any (\o -> ((valueOf (txOutValue o) adaSymbol adaToken) == adaAmount + adaR)) txOuts
      -- flattenValue:: Value -> [(CurrencySymbol,TokenName,Integer)]
      -- txOutValue champ contenant la Value dans TxOut
      -- scriptOuput2 correspond à l'output verifiant que le script a vraiment
      -- recu tokenAmount
      scriptOuput2 =
        P.any
          ( \o ->
              P.any
                ( \(currSymb, tokN, amount) ->
                    (currSymb P./= adaSymbol) P.&& (amount == tokenAmount + tokR)
                )
                (flattenValue (txOutValue o))
          )
          txOuts
      -- ownerOuput correspond à l'output correspondant
      -- a celui dans lequel le LP a recu les LP tokens ou tokensForLp
      -- --verifier plutot dans le mint
      mint = txInfoMint (scriptContextTxInfo ctx)
      ownerMinted = valueOf  mint lpCurrSymb lpTokName == tokensForLp
      -- ownerMinted =
      --   P.any
      --     ( \o ->
      --         ((valueOf (txOutValue o) lpCurrSymb lpTokName) == tokensForLp)
      --           P.&& ( case addressCredential (txOutAddress o) of
      --                    PubKeyCredential ownerPKH -> ownerPKH == lpPKH
      --                    _ -> False
      --                )
      --     )
      --     (txInfoOutputs (scriptContextTxInfo ctx))
   in (scriptOuput1 P.&& scriptOuput2 P.&& ownerMinted)

-- swap part
{-# INLINEABLE swap #-}
-- Take 1% on each swap
-- Before swap we must verify off-chain if there is enough of tokens for the
-- swap
swap :: Amount -> Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
-- amount -> amount to swap
-- minAmount -> minimum token expected
-- tokSymbol == adaSymbol -> that mean the user want to swap ada to token
swap amount minAmount adaR tokR tokSymbol ctx =
  P.and conditions
  where
    -- swaps ada with tokens

    conditions :: [Bool]
    conditions =
      [ userInput amount tokSymbol ctx,
        userOutput amount minAmount adaR tokR tokSymbol ctx,
        scriptOutput amount adaR tokR tokSymbol ctx
      ]
    -- tokSymbol can be adaSymbol or the TokenSymbol. It depends on what user wants to swap.
    userInput :: Amount -> TokenSymbol -> ScriptContext -> Bool
    userInput amount tokSymbol ctx =
      let userPKH = P.head $ txInfoSignatories (scriptContextTxInfo ctx)
          inputs = txInfoInputs $ scriptContextTxInfo ctx
       in -- find the good input at the user pkh with the good amount
          P.any
            ( \inp ->
                case addressCredential (txOutAddress $ txInInfoResolved inp) of
                  PubKeyCredential pkh ->
                    pkh
                      == userPKH
                      P.&& P.any
                        (\(cs, _, am) -> (cs P.== tokSymbol) P.&& (am P.>= amount))
                        (flattenValue $ txOutValue $ txInInfoResolved inp)
                  _ -> False
            )
            inputs

    userOutput :: Amount -> Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
    userOutput amount minAmount adaR tokR tokSymbol ctx =
      case tokSymbol P.== adaSymbol of
        True ->
          let tokensBougth = adaToToken amount adaR tokR
              pkh = P.head $ txInfoSignatories (scriptContextTxInfo ctx)
           in if tokensBougth < minAmount
                then False
                -- Verify that user get the good tokens amount
                -- in output
                else
                  P.any
                    ( \o ->
                        P.any
                          (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokensBougth))
                          (flattenValue $ txOutValue $ o)
                          P.&& ( case addressCredential (txOutAddress o) of
                                   PubKeyCredential userPKH -> userPKH P.== pkh
                                   _ -> False
                               )
                    )
                    (txInfoOutputs (scriptContextTxInfo ctx))
        False ->
          let adaBougth = tokenToAda amount tokR adaR
              userPKH = P.head $ txInfoSignatories (scriptContextTxInfo ctx)
           in if adaBougth < minAmount
                then False
                -- Verify that user get the good tokens amount
                -- in output
                else
                  P.any
                    ( \o ->
                        valueOf (txOutValue o) adaSymbol adaToken
                          P.== adaBougth
                          P.&& ( case addressCredential (txOutAddress o) of
                                   PubKeyCredential pkh -> pkh == userPKH
                                   _ -> False
                               )
                    )
                    (txInfoOutputs (scriptContextTxInfo ctx))

    scriptOutput :: Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
    scriptOutput amount adaR tokR tokSymbol ctx =
      case getContinuingOutputs ctx of
        [o] -> case tokSymbol P.== adaSymbol of
          True ->
            let tokensBougth = adaToToken amount adaR tokR
                -- update the ada's reserve because it swap ada and get tokens
                -- so that token's reserve decrease and ada's reserve increase
                adaReserveUpdate = valueOf (txOutValue o) adaSymbol adaToken P.== adaR P.+ amount
                tokenReserveUpdate =
                  P.any
                    (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokR P.- tokensBougth))
                    (flattenValue $ txOutValue $ o)
             in adaReserveUpdate P.&& tokenReserveUpdate
          False ->
            let adaBougth = tokenToAda amount tokR adaR
                -- update the token's reserve because it gives tokens and get ada
                -- so that token's reserve increase and ada's reserve decrease
                adaReserveUpdate = valueOf (txOutValue o) adaSymbol adaToken P.== adaR P.- adaBougth
                tokenReserveUpdate =
                  P.any
                    (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokR P.+ amount))
                    (flattenValue $ txOutValue $ o)
             in adaReserveUpdate P.&& tokenReserveUpdate
        _ -> False

{-# INLINEABLE adaToToken #-}
adaToToken :: Amount -> AdaReserve -> TokenReserve -> Amount
adaToToken amount adaR tokR =
  let tokensBougth = getAmountForSwp amount adaR tokR
   in tokensBougth

{-# INLINEABLE tokenToAda #-}
tokenToAda :: Amount -> TokenReserve -> AdaReserve -> Amount
tokenToAda amount tokR adaR =
  let adaBougth = getAmountForSwp amount tokR adaR
   in adaBougth

{-# INLINEABLE getAmountForSwp #-}
-- get 1% of fee on each swap
getAmountForSwp :: Amount -> Amount -> Amount -> Amount
getAmountForSwp inputAmount inputReserve outputReserve =
  let inputAmountWithFee = inputAmount P.* 99
      numerator = inputAmountWithFee * outputReserve
      denominator = (inputReserve P.* 100) P.+ inputAmountWithFee
   in P.divide numerator denominator

--
{-# INLINEABLE ammUntypedValidator #-}
ammUntypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
ammUntypedValidator _ redeemer ctx =
  -- check retourne ()
  P.check
    ( ammValidator
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||ammUntypedValidator||])

cip57 :: IO ()
cip57 = writeValidatorToFile "a2.plutus" validator
