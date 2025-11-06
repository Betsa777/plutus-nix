{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- import AMMSpec qualified as AMMSpec
-- import CGPlutusUtilsSpec qualified as CGPlutusUtilsSpec
-- import CGTimeSpec qualified as CGTimeSpec
-- import DemoSpec qualified as DemoSpec
-- import ParameterizedVestingSpec qualified as ParameterizedVestingSpec
-- import Test.Tasty (TestTree, defaultMain, testGroup)
-- import VestingSpec qualified as VestingSpec

-- main :: IO ()
-- main = defaultMain allTests

-- allTests :: TestTree
-- allTests =
--   testGroup
--     "All wspace Tests"
--     [ AMMSpec.tests
--     -- VestingSpec.tests,
--     --   ParameterizedVestingSpec.tests,
--     --   CGTimeSpec.tests,
--     --   CGPlutusUtilsSpec.tests,
--     --   DemoSpec.tests
--     ]

import AMM (AMMRedeemer (..), ammValidator)
import Data.ByteString.Char8 qualified as C
import Plutus.V1.Ledger.Api (TxId (..))
import Plutus.V1.Ledger.Interval (interval)
import Plutus.V1.Ledger.Scripts (ValidatorHash (..))
import Plutus.V1.Ledger.Value
  ( CurrencySymbol (..),
    TokenName (..),
    adaSymbol,
    adaToken,
    singleton,
  )
import Plutus.V2.Ledger.Api
  ( Address (..),
    Credential (..),
    OutputDatum (..),
    PubKeyHash (..),
    ScriptContext (..),
    ScriptHash (..),
    addressCredential,
    scriptContextTxInfo,
  )
import Plutus.V2.Ledger.Contexts
  ( ScriptPurpose (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Test.Hspec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

adaAmount :: Integer
adaAmount = 100

tokenAmount :: Integer
tokenAmount = 200

adaR :: Integer
adaR = 200

tokR :: Integer
tokR = 0

lpTokNumb :: Integer
lpTokNumb = 10

lpPKH :: PubKeyHash
lpPKH = PubKeyHash $ BuiltinByteString (C.pack "lp")

lpCurrSymb :: CurrencySymbol
lpCurrSymb = CurrencySymbol $ BuiltinByteString (C.pack "LP")

lpTokName :: TokenName
lpTokName = TokenName $ BuiltinByteString (C.pack "LPToken")

tokCS :: CurrencySymbol
tokCS = CurrencySymbol $ BuiltinByteString (C.pack "WIMS")

tokName :: TokenName
tokName = TokenName $ BuiltinByteString (C.pack "WimsToken")

-- Remplace ScriptHash par ValidatorHash
scriptAddress = Address (ScriptCredential (ValidatorHash (BuiltinByteString (C.pack "script")))) Nothing

lpAddress = Address (PubKeyCredential (PubKeyHash (BuiltinByteString (C.pack "lp")))) Nothing

scriptOutput :: TxOut
scriptOutput =
  TxOut
    { txOutAddress = scriptAddress,
      txOutValue =
        singleton adaSymbol adaToken adaAmount
          <> singleton tokCS tokName tokenAmount,
      txOutDatum = NoOutputDatum,
      txOutReferenceScript = Nothing
    }

value = singleton adaSymbol adaToken adaAmount

-- comme tokR == 0 donc tokensForLp = adaR + adaAmount
lpOutput =
  TxOut
    { txOutAddress = lpAddress,
      txOutValue = singleton lpCurrSymb lpTokName (adaR + adaAmount),
      txOutDatum = NoOutputDatum,
      txOutReferenceScript = Nothing
    }

txInfo :: TxInfo
txInfo =
  TxInfo
    { txInfoInputs = [],
      txInfoReferenceInputs = [],
      txInfoOutputs = [lpOutput, scriptOutput],
      txInfoFee = value,
      txInfoMint = value,
      txInfoDCert = [],
      txInfoWdrl = Map.empty,
      txInfoValidRange = interval 10 20,
      txInfoSignatories = [lpPKH],
      txInfoRedeemers = Map.empty,
      txInfoData = Map.empty,
      txInfoId = TxId $ BuiltinByteString (C.pack "")
    }

scriptContext :: ScriptContext
scriptContext =
  ScriptContext
    { scriptContextTxInfo = txInfo,
      scriptContextPurpose = Spending $ TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0
    }

redeemer :: AMMRedeemer
redeemer = AddLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName

main :: IO ()
main = hspec $ do
  describe "AMMValidator" $ do
    it "must give true" $ do
      ammValidator redeemer scriptContext
        `shouldBe` True
