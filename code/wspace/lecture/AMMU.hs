{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AMMU where

-- import Plutus.V1.Ledger.Api ()

import GHC.Generics (Generic)
import Plutus.V1.Ledger.Value
  ( CurrencySymbol (..),
    TokenName (..),
    Value (..),
    adaSymbol,
    adaToken,
    flattenValue,
    symbols,
    valueOf,
  )
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
  )
import Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.Prelude as P
import PlutusTx.Sqrt (Sqrt (..), isqrt)
import Utilities (writeValidatorToFile)
import Prelude (IO, Show)

data PoolDatum = PoolDatum
  { resA :: Integer,
    resB :: Integer,
    lpTken :: Value,
    poolNFT :: Value
  }

PlutusTx.makeLift ''PoolDatum
PlutusTx.makeIsDataIndexed
  ''PoolDatum
  [('PoolDatum, 0)]

data AMMRedeemer = AddLiq Integer Integer | Swap {inA :: Bool} | RemoveLiq

PlutusTx.makeLift ''AMMRedeemer
PlutusTx.makeIsDataIndexed
  ''AMMRedeemer
  [ ('AddLiq, 0),
    ('Swap, 1),
    ('RemoveLiq, 2)
  ]

{-# INLINEABLE ammValidator #-}
ammValidator :: PoolDatum -> AMMRedeemer -> ScriptContext -> Bool
ammValidator d r ctx =
  case r of
    AddLiq amA amB ->
      P.traceIfFalse "Liquidty adding fails" (addLiquidity amA amB d r ctx)


{-# INLINEABLE verifyDatum #-}
verifyDatum:: Integer -> Integer -> PoolDatum -> PoolDatum -> Bool 
verifDatum amA amB oldD newD = 
  (poolNFT oldD P.== poolNFT newD)
  P.&& (resA oldD P.== resA newD)
  P.&& (resB oldD P.== resB newD)
  P.&& (feeBps oldD P.== feeBps newD)
  P.&& (lptokNumb oldD P.== lptokNumb newD)
  P.&& (lpPolicyId oldD P.== lpPolicyId newD)

{-# INLINEABLE addLiquidity #-}
addLiquidity :: Integer -> Integer -> PoolDatum -> AMMRedeemer -> ScriptContext -> Bool
addLiquidity amA amB d r ctx =
  let newDatum = case getContinuingOutputs ctx of
        [o] ->
          case (txOutDatum o) of
            OutputDatumHash datumHash -> let findDatum datumHash (scriptContextTxInfo ctx)
            OutputDatum (Datum d') ->
              let datum = PlutusTx.unsafeFromBuiltinData d' :: PoolDatum
               in if poolNFT datum
                    P.== poolNFT d
                    P.&& resA datum
                    P.== resA d
                    P.+ amA
                    P.&& resB datum
                    P.== resB d
                    P.+ amB
                    -- verifier aussi que le script a recu les 2 tokens
                    then (Just datum)
                    else Nothing
            _ -> Nothing
        _ -> Nothing

      scriptOuput = case getContinuingOutputs ctx of
        [o] ->
          -- je m'assure que la sortie a exactement 2 tokens
          if (P.length $ flattenValue $ txOutValue o) P./= 2
            then False
            else
              -- flattenValue:: Value -> [(CurrencySymbol,TokenName,Integer)]
              let fl = flattenValue $ txOutValue o
                  (_, _, newRA) = head fl
                  (_, _, newRB) = fl !! 1
               in -- Je m'assure que les resrves correspondent bien aux montants
                  -- des tokens  dans le pool
                  (((resA d) P.+ amA P.== newRA) P.&& ((resB d) + amB P.== newRB))
                    P.|| (((resA d) P.+ amA P.== newRB) P.&& ((resB d) + amB P.== newRA))
        _ -> False

      result = case newDatum of
        Just d' ->
          let lpPKHList = txInfoSignatories $ scriptContextTxInfo ctx
           in if (P.length lpPKHList) P./= 1
                then Nothing
                else
                  let [lpOuput] =
                        P.filter
                          ( \o ->
                              case (addressCredential $ txOutAddress o) of
                                PubKeyCredential pkh -> pkh == head lpPKHList
                                _ -> False
                          )
                          (txInfoOutputs $ (scriptContextTxInfo ctx))
                   in case P.filter (\(c, _, _) -> c P.== (lpPolicyId d)) (flattenValue $ txOutValue lpOuput) of
                        [(_, _, alp)] ->
                          if (lptokNumb d) P.== 0
                            then
                              let sqrt = isqrt (amA P.* amB)
                               in case sqrt of
                                    Exactly s -> Just (alp P.<= s)
                                    Approximately s -> Just (alp P.<= s)
                                    _ -> Just False
                            else
                              Just
                                ( alp
                                    P.<= P.min
                                      (P.divide (amA P.* (lptokNumb d)) (resA d))
                                      (P.divide (amB P.* (lptokNumb d)) (resB d))
                                )
                        _ -> Just False
        _ -> Just False
   in case result of
        Just r -> scriptOuput P.&& r

-- {-# INLINEABLE isqrt #-}
-- isqrt :: Integer -> Integer
-- isqrt n = go n
--   where
--     go x
--       | x * x <= n = x
--       | otherwise = go (x - 1)
