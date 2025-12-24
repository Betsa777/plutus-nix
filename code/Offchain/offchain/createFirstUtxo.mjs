import { lucid, validatorAddress, walletAddress } from "./common.mjs";
import { getHash } from "./getHash.js";
import { Constr, Data } from "lucid-cardano";
console.log(validatorAddress);

async function createfirstUtxo() {
  let adaR = 2_000_000n; // en ADA
  let tokR = 0n;
  let datumHash = getHash(adaR, tokR);
  // Construire la transaction
  const tx = await lucid
    .newTx()
    .payToContract(
      validatorAddress,
      { inline: Data.to(new Constr(0, [])) },
      { lovelace: 2_000_000n },
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log("Tx hash:", txHash);
}

createfirstUtxo();
//33f1bd5babd366834352add2341b14f8d3bda733a4d1d6ba5c4aa73864e05e65
