import { lucid, validatorAddress, walletAddress } from "./common.mjs";
import { Constr, Data, toHex, fromHex } from "lucid-cardano";
console.log(validatorAddress);
let pkhHex =
  lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

const poolDatum = new Constr(0, [
  [toHex(""), toHex(""), 2_000_000n], // reserves (ici seulement ADA pour tester)
  [toHex(""), toHex("TokenA")], // tokenA = (CurrencySymbol, TokenName)
  [toHex(""), toHex("TokenB")], // tokenB = (CurrencySymbol, TokenName)
  [toHex(""), toHex("LPToken")], // lpToken = (CurrencySymbol, TokenName)
  toHex(pkhHex), // lpPKH en format hex ou bech32
]);
async function createfirstUtxo() {
  let adaR = 2_000_000n; // en ADA
  let tokR = 0n;
  // Construire la transaction
  const tx = await lucid
    .newTx()
    .payToContract(
      validatorAddress,
      { inline: Data.to(poolDatum) },
      { lovelace: 2_000_000n },
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log("Tx hash:", txHash);
}

createfirstUtxo();
//33f1bd5babd366834352add2341b14f8d3bda733a4d1d6ba5c4aa73864e05e65
