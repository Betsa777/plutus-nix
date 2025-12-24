import { mConStr0, mConStr1, stringToHex, UTxO } from "@meshsdk/common/dist";
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
  user_wallet,
} from "./common";
import { MeshWallet } from "@meshsdk/wallet/dist";
import { deserializeAddress } from "@meshsdk/core/dist";

// /**
//  * @notice Cette fonction permet d'ajouter un candidat à l'election
//  * @param utxo UTxO de la transaction précedente à l'adresse du smart contract
//  */
export async function swapWimsTokenToAda(utxo: UTxO) {
  const utxos = await user_wallet.getUtxos();

  let collateral = await user_wallet.getCollateral();
  if (collateral.length == 0) await createCollateral();

  collateral = await user_wallet.getCollateral();
  //console.log({ collateral });
  let collateralGot = collateral[0];
  let collateralInput = collateralGot.input;
  let collateralOutput = collateralGot.output;
  console.log("user address ", user_wallet.addresses.baseAddressBech32);

  const key = deserializeAddress(
    //@ts-ignore
    user_wallet.addresses.baseAddressBech32?.toString(),
  );

  console.log({ key });
  const scriptUtxos = utxo;
  console.log("script tokens ", scriptUtxos.output.amount);

  // const
  const adaReserve = BigInt(
    utxo.output.amount.find((a) => a.unit === "lovelace")?.quantity || 0,
  );
  const wimsObj = utxo.output.amount.find((a) => a.unit === WIMSASSET_ID);
  console.log({ wimsObj });
  const wimsReserve = BigInt(wimsObj?.quantity || 0n);

  console.log("ADA reserve:", adaReserve);
  console.log("WIMS reserve:", wimsReserve);
  const wimsAmount = 10_000n; // 1ADA côté
  const minAmount = 100n;

  console.log("scriptAddr", scriptAddr);
  //Swap Amount Amount AdaReserve TokenReserve TokenSymbol TokenSymbol
  // Ici TokenIn = WimsToken et TokenOut = Ada
  const redeemer = mConStr1([
    wimsAmount,
    minAmount,
    adaReserve,
    wimsReserve,
    POLICY_ID,
    stringToHex(""),
  ]);
  const inputAmountWithFee = wimsAmount * 99n;
  const numerator = inputAmountWithFee * adaReserve;
  const denominator = wimsReserve * 100n + inputAmountWithFee;
  const adaBought = numerator / denominator;
  console.log({ adaBought });
  const txBuilder = await getTxBuilder();
  const txOuts = [
    {
      unit: "lovelace",
      quantity: (adaReserve - adaBought).toString(),
    },
    {
      unit: WIMSASSET_ID,
      quantity: (wimsReserve + wimsAmount).toString(),
    },
  ];
  const userOut = [
    {
      unit: "lovelace",
      quantity: adaBought.toString(),
    },
  ];
  // //   console.log(txOuts);

  console.log("the script utxo is ", utxo);
  if (!user_wallet.addresses.baseAddressBech32)
    throw new Error("user wallet undefined");

  // Find a UTXO that contains the WIMS tokens we want to swap
  const wimsUtxo = utxos.find((utxo) => {
    const wimsInUtxo = utxo.output.amount.find((a) => a.unit === WIMSASSET_ID);
    return wimsInUtxo && BigInt(wimsInUtxo.quantity) >= wimsAmount;
  });

  if (!wimsUtxo) {
    throw new Error(`No UTXO found with at least ${wimsAmount} WIMS tokens`);
  }
  // DEBUG CRITIQUE : Vérifier l'adresse de l'UTXO WIMS
  console.log("WIMS UTXO address:", wimsUtxo.output.address);
  console.log("User wallet address:", user_wallet.addresses.baseAddressBech32);
  console.log(
    "Addresses match:",
    wimsUtxo.output.address === user_wallet.addresses.baseAddressBech32,
  );
  console.log({ wimsUtxo });
  const tx = await txBuilder
    // Ajouter l'input des tokens WIMS de l'utilisateur
    // .txIn(
    //   wimsUtxo.input.txHash,
    //   wimsUtxo.input.outputIndex,
    //   wimsUtxo.output.amount,
    //   wimsUtxo.output.address,
    // )
    .spendingPlutusScriptV2()
    .txIn(
      utxo.input.txHash,
      utxo.input.outputIndex,
      utxo.output.amount,
      scriptAddr,
    )
    .txOut(scriptAddr, txOuts) // Sortie au script
    .txOutInlineDatumValue(1n) // MAINTENANT ceci s'applique à la sortie du script
    // .txOut(owner_wallet.addresses.baseAddressBech32, lpOut) // Sortie LP tokens à l'utilisateur
    .txOut(user_wallet.addresses.baseAddressBech32, userOut)
    .txInScript(scriptCbor)
    .txInInlineDatumPresent()
    .txInRedeemerValue(redeemer)
    .txInCollateral(
      collateralInput.txHash,
      collateralInput.outputIndex,
      collateralOutput.amount,
      collateralOutput.address,
    )
    .selectUtxosFrom(utxos)
    .requiredSignerHash(key.pubKeyHash)
    .changeAddress(user_wallet.addresses.baseAddressBech32)
    .complete();

  const txSigned = await user_wallet.signTx(tx);
  try {
    const txHash = await user_wallet.submitTx(txSigned);
    console.log({ txHash });
  } catch (error) {
    console.log(error);
  }
}
//6a6819f08149834db80544bbb1c95e60f84f69217e206982e06c5c96c6a4e03e
//69e4c0b24cb26a94a81f125934071e21889f30920f522773462ced18b3e48604
