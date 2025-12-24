import {
  mConStr0,
  mConStr1,
  mConStr2,
  NativeScript,
  pubKeyHash,
  stringToHex,
  UTxO,
} from "@meshsdk/common/dist";
import {
  blockchainProvider,
  createCollateral,
  getPrecedentDatum,
  getTxBuilder,
  owner_wallet,
  scriptAddr,
  scriptCbor,
  getUtxosAtScript,
  WIMSASSET_ID,
  lpTokensNumber,
  LPTOKENName,
  LPASSET_ID,
  POLICY_ID,
  assets,
  WIMSTOKENName,
} from "./common";
import { MeshWallet } from "@meshsdk/wallet/dist";
import {
  deserializeAddress,
  ForgeScript,
  resolveScriptHash,
} from "@meshsdk/core/dist";

// /**
//  * @notice Cette fonction permet d'ajouter un candidat à l'election
//  * @param utxo UTxO de la transaction précedente à l'adresse du smart contract
//  */
export async function removeLiquidity(utxo: UTxO) {
  const utxos = await owner_wallet.getUtxos();

  let collateral = await owner_wallet.getCollateral();
  if (collateral.length == 0) await createCollateral();

  collateral = await owner_wallet.getCollateral();
  //console.log({ collateral });
  let collateralGot = collateral[0];
  let collateralInput = collateralGot.input;
  let collateralOutput = collateralGot.output;

  const key = deserializeAddress(
    //@ts-ignore
    owner_wallet.addresses.baseAddressBech32?.toString(),
  );
  // console.log({ owner_wallet });
  // console.log({ key });
  // const scriptUtxos = utxo;
  // console.log({ scriptUtxos });

  // const
  const adaReserve = BigInt(
    utxo.output.amount.find((a) => a.unit === "lovelace")?.quantity || 0,
  );
  const wimsObj = utxo.output.amount.find((a) => a.unit === WIMSASSET_ID);
  console.log({ wimsObj });
  const wimsReserve = BigInt(wimsObj?.quantity || 0n);

  console.log("ADA reserve:", adaReserve);
  console.log("WIMS reserve:", wimsReserve);
  const amount = 2000000n;
  //RemoveLiquidity Amount AdaReserve TokenReserve LPTokenNumber LPTokenCurrencySymbol LPTokenName TokenSymbol TokenName

  const nativeScript: NativeScript = {
    type: "all",
    scripts: [{ type: "sig", keyHash: key.pubKeyHash }],
  };
  const forgeScript = ForgeScript.fromNativeScript(nativeScript);
  const lpPolicyId = resolveScriptHash(forgeScript);

  const redeemer = mConStr2([
    amount,
    adaReserve,
    wimsReserve,
    lpTokensNumber,
    lpPolicyId,
    stringToHex(LPTOKENName),
    POLICY_ID,
    stringToHex(WIMSTOKENName),
  ]);
  const adaAmount = (adaReserve * amount) / lpTokensNumber;
  const wimsAmount = (wimsReserve * amount) / lpTokensNumber;
  const txBuilder = await getTxBuilder();
  const txOuts = [
    {
      unit: "lovelace",
      quantity: (adaReserve - adaAmount).toString(),
    },
    {
      unit: WIMSASSET_ID,
      quantity: (wimsReserve - wimsAmount).toString(),
    },
  ];
  const lpOut = [
    {
      unit: "lovelace",
      quantity: adaAmount.toString(),
    },
    {
      unit: WIMSASSET_ID,
      quantity: wimsAmount.toString(),
    },
  ];
  console.log(txOuts);
  let ownerLPAmount = 0n;
  //@ts-ignore
  for (let i = 0; utxos < utxos.length; i++) {
    ownerLPAmount += BigInt(
      //@ts-ignore
      utxos[i].output.amount.find(
        //@ts-ignore
        (a) => a.unit === lpPolicyId + stringToHex(LPTOKENName),
      )?.quantity || 0,
    );
  }
  console.log({ ownerLPAmount });

  console.log("the script utxo is ", utxo);
  if (!owner_wallet.addresses.baseAddressBech32) throw new Error("address");
  const tx = await txBuilder
    .spendingPlutusScriptV2()
    .txIn(
      utxo.input.txHash,
      utxo.input.outputIndex,
      utxo.output.amount,
      scriptAddr,
    )
    .txOut(scriptAddr, txOuts) // Sortie au script
    .txOutInlineDatumValue(1n) // MAINTENANT ceci s'applique à la sortie du script
    .txOut(owner_wallet.addresses.baseAddressBech32, lpOut) // Sortie Montants retirés
    .txInScript(scriptCbor)
    .txInInlineDatumPresent()
    .txInRedeemerValue(redeemer)
    .txInCollateral(
      collateralInput.txHash,
      collateralInput.outputIndex,
      collateralOutput.amount,
      collateralOutput.address,
    )
    .mint((-amount).toString(), lpPolicyId, stringToHex(LPTOKENName))
    .mintingScript(forgeScript)
    .selectUtxosFrom(utxos)
    .changeAddress(owner_wallet.addresses.baseAddressBech32)
    .complete();

  const txSigned = await owner_wallet.signTx(tx);
  try {
    const txHash = await owner_wallet.submitTx(txSigned);
    console.log({ txHash });
  } catch (error) {
    console.log(error);
  }
}
