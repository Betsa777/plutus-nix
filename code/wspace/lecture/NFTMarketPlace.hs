{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NFTMarketPlace where

import Plutus.V1.Ledger.Value
  ( CurrencySymbol (..),
    TokenName (..),
    Value (..),
    adaSymbol,
    adaToken,
    flattenValue,
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
    txOutValue
  )
import Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Contexts (getContinuingOutputs,findDatum,txSignedBy,pubKeyOutputsAt)
import PlutusTx
import PlutusTx.Prelude as P
import PlutusTx.Sqrt (Sqrt (..), isqrt)
import Utilities (writeValidatorToFile)
import Prelude (IO)

data MDatum = MDatum{
    price:: Integer,
    nftCs:: CurrencySymbol,
    nftTn:: TokenName,
    seller:: PubKeyHash
}
PlutusTx.makeIsDataIndexed ''MDatum [('MDatum, 0)]
PlutusTx.makeLift ''MDatum

data MRedeemer = Sell Integer CurrencySymbol TokenName PubKeyHash| Buy PubKeyHash | Update Integer | Cancel 
PlutusTx.makeIsDataIndexed ''MRedeemer [
                              ('Sell, 0),
                              ('Buy,1),
                              ('Update,2),
                              ('Cancel,3)]
PlutusTx.makeLift ''MRedeemer

{-# INLINEABLE mValidator #-}
mValidator:: MDatum -> MRedeemer -> ScriptContext -> Bool 
mValidator mDatum mRedeemer ctx = 
    case mRedeemer of
        Sell price' nftCs' nftTn' seller' -> case getContinuingOutputs ctx of 
                                          -- check in the script outout that the datum is correct
                                          --  match the redeemer values
                                          -- and verify that the seller signed the transaction
                                     [o] -> case   txOutDatum o  of
                                              OutputDatum (Datum datum) -> let d = (PlutusTx.unsafeFromBuiltinData datum) :: MDatum 
                                                                            in (price d) == price' &&
                                                                               (nftCs d) == nftCs' &&
                                                                               (nftTn d) == nftTn' &&
                                                                               (seller d) == seller' &&
                                                                               valueOf (txOutValue o) nftCs' nftTn' == 1 && 
                                                                               txSignedBy (scriptContextTxInfo ctx) seller'
                                              OutputDatumHash dHash -> case findDatum dHash (scriptContextTxInfo ctx) of 
                                                                        Just (Datum datum) -> let d = PlutusTx.unsafeFromBuiltinData datum :: MDatum 
                                                                                               in (price d) == price' &&
                                                                                                  (nftCs d) == nftCs' &&
                                                                                                  (nftTn d) == nftTn' &&
                                                                                                  (seller d) == seller' &&
                                                                                                  valueOf (txOutValue o) nftCs' nftTn' == 1 &&
                                                                                                  txSignedBy (scriptContextTxInfo ctx) seller'
                                                                        Nothing    -> False
                                                                       
                                              NoOutputDatum -> False
                                     _ -> False


        Update newPrice -> case getContinuingOutputs ctx of 
                        -- check in the script outout that the datum is correct with the
                        -- new price and the same nft and seller stay the same
                        [o] -> case   txOutDatum o  of
                                 OutputDatum (Datum datum) -> let d = (PlutusTx.unsafeFromBuiltinData datum) :: MDatum 
                                                               in (price d) == newPrice &&
                                                                  (nftCs d) == (nftCs mDatum) &&
                                                                  (nftTn d) == (nftTn mDatum) &&
                                                                  (seller d) == (seller mDatum) &&
                                                                  txSignedBy (scriptContextTxInfo ctx) (seller mDatum) 
                                                                  

                                 OutputDatumHash dHash -> case findDatum dHash (scriptContextTxInfo ctx) of 
                                                           Just (Datum datum) -> let d = PlutusTx.unsafeFromBuiltinData datum :: MDatum 
                            
                                                                                  in (price d) == newPrice &&
                                                                                     (nftCs d) == (nftCs mDatum) &&
                                                                                     (nftTn d) == (nftTn mDatum) &&
                                                                                     (seller d) == (seller mDatum) &&
                                                                                     txSignedBy (scriptContextTxInfo ctx) (seller mDatum) 
                                                                                     
                                                           Nothing    -> False
                                 NoOutputDatum -> False
                        _ -> False

        Cancel ->  case getContinuingOutputs ctx of
                        -- check that there is no more output in the script
                        [] -> let sellerSignature = txSignedBy (scriptContextTxInfo ctx) (seller mDatum)
                                  sellerOutputsValues = pubKeyOutputsAt (seller mDatum) (scriptContextTxInfo ctx)
                              in sellerSignature && 
                                any (\v -> let nftList = getNftData v
                                             in ((nftCs mDatum),nftTn mDatum) `elem` nftList) sellerOutputsValues
                        _  -> False

        Buy buyer -> 
           and conditions 
            where 
             conditions :: [Bool]
             conditions = [
               buyerInput buyer ctx mDatum,
               buyerOutput buyer ctx mDatum,
               sellerOutput buyer ctx mDatum,
               scriptOutput ctx
               ]
             -- Check that the buyer has provided enough input to cover the price
             -- Check that he signed the transaction
             buyerInput :: PubKeyHash -> ScriptContext -> MDatum -> Bool
             buyerInput buyer ctx mDatum =
               let txInputs = txInfoInputs (scriptContextTxInfo ctx)
                   txSign = txSignedBy (scriptContextTxInfo ctx) buyer
                   goodInput = any (\i -> valueOf (txOutValue (txInInfoResolved i )) adaSymbol adaToken >= (price mDatum)
                                          && case addressCredential $ txOutAddress (txInInfoResolved i ) of 
                                                PubKeyCredential buyerPkh -> buyerPkh == buyer
                                                _ -> False
                                   ) txInputs
                in txSign && goodInput
             
             -- Check that the buyer receives the NFT in one of his outputs
             buyerOutput :: PubKeyHash -> ScriptContext -> MDatum -> Bool
             buyerOutput buyer ctx mDatum =
              let txOutputs = txInfoOutputs (scriptContextTxInfo ctx)
                  txSign = txSignedBy (scriptContextTxInfo ctx) buyer
                  buyerOutputValue = pubKeyOutputsAt buyer (scriptContextTxInfo ctx)
                  goodValue = any (\v -> valueOf v (nftCs mDatum) (nftTn mDatum) == 1) buyerOutputValue
                in  txSign && goodValue
            
             -- Check that the seller receives the correct ada amount in one of his outputs
             sellerOutput :: PubKeyHash -> ScriptContext -> MDatum -> Bool
             sellerOutput buyer ctx mDatum =
                let txOutputs = txInfoOutputs (scriptContextTxInfo ctx)
                    sellerPkh = seller mDatum
                    sellerOutputValue = pubKeyOutputsAt sellerPkh (scriptContextTxInfo ctx)
                    goodValue = any (\v -> valueOf v adaSymbol adaToken >= (price mDatum)) sellerOutputValue
                  in  goodValue
              
              -- Check that there is no more output in the script
             scriptOutput :: ScriptContext -> Bool
             scriptOutput ctx =
                 length (getContinuingOutputs ctx) == 0
         



{-# INLINEABLE getNftData #-}
getNftData:: Value -> [(CurrencySymbol, TokenName)]
getNftData nftValue = 
  [(cs,tn) | (cs,tn,amt) <- flattenValue nftValue, amt == 1, cs /= adaSymbol,tn /= adaToken]
  

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator mDatum mRedeemer ctx =
  -- check retourne ()
  P.check
    ( mValidator
        (PlutusTx.unsafeFromBuiltinData mDatum)
        (PlutusTx.unsafeFromBuiltinData mRedeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

validatorScript :: PlutusV2.Validator
validatorScript = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||untypedValidator||])

getCbor :: IO ()
getCbor = writeValidatorToFile "./assets/marketplace.plutus" validatorScript 
                          
