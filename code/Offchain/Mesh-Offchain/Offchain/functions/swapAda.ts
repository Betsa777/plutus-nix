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
export async function swapAdaToWimsToken(utxo: UTxO) {
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
  console.log({ scriptUtxos });

  // const
  const adaReserve = BigInt(
    utxo.output.amount.find((a) => a.unit === "lovelace")?.quantity || 0,
  );
  const wimsObj = utxo.output.amount.find((a) => a.unit === WIMSASSET_ID);
  console.log({ wimsObj });
  const wimsReserve = BigInt(wimsObj?.quantity || 0n);

  console.log("ADA reserve:", adaReserve);
  console.log("WIMS reserve:", wimsReserve);
  const adaAmount = 2_000_000n; // 1ADA côté
  const minAmount = 5000n;

  console.log("scriptAddr", scriptAddr);
  //Swap Amount Amount AdaReserve TokenReserve TokenSymbol TokenSymbol
  // Ici TokenIn = Ada et TokenOut = WimsTokens
  const redeemer = mConStr1([
    adaAmount,
    minAmount,
    adaReserve,
    wimsReserve,
    stringToHex(""),
    POLICY_ID,
  ]);
  const inputAmountWithFee = adaAmount * 99n;
  const numerator = inputAmountWithFee * wimsReserve;
  const denominator = adaReserve * 100n + inputAmountWithFee;
  const tokenBought = numerator / denominator;
  console.log({ tokenBought });
  const txBuilder = await getTxBuilder();
  const txOuts = [
    {
      unit: "lovelace",
      quantity: (adaReserve + adaAmount).toString(),
    },
    {
      unit: WIMSASSET_ID,
      quantity: (wimsReserve - tokenBought).toString(),
    },
  ];
  const userOut = [
    {
      unit: WIMSASSET_ID,
      quantity: tokenBought.toString(),
    },
  ];
  // //   console.log(txOuts);

  console.log("the script utxo is ", utxo);
  if (!user_wallet.addresses.baseAddressBech32)
    throw new Error("user wallet undefined");
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
