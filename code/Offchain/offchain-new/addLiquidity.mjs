import { Constr, Data, toHex } from "lucid-cardano";
import {
  lucid,
  walletAddress,
  validatorAddress,
  validator,
} from "./common.mjs";
import { getHash } from "./getHash.js";
import { lpTokensNumber } from "./lPTokens.js";
import {
  WIMSASSET_ID,
  LPASSET_ID,
  WIMSTOKENName,
  LPTOKENName,
  POLICY_ID,
  mintingPolicy,
} from "./mint/mintTokens.mjs";
import { PoolParams } from "./types.js";

async function getPoolState() {
  const utxos = await lucid.utxosAt(validatorAddress);
  console.log("UTxOs at script:", utxos);

  let adaReserve = 0n;
  let wimsReserve = 0n;

  for (const utxo of utxos) {
    adaReserve += utxo.assets.lovelace ?? 0n;
    wimsReserve += utxo.assets[WIMSASSET_ID] ?? 0n;
  }

  console.log("ADA reserve:", adaReserve);
  console.log("WIMS reserve:", wimsReserve);
  return { adaReserve, wimsReserve };
}
let { adaReserve, wimsReserve } = await getPoolState();
// Paramètres du pool et de l’ajout
const adaAmount = 1_000_000n; // 1ADA côté
const wimsAmount = 50_000n; // WIMSTOKEN que l'utilisateur envoie
//LPTokens minted must be save on=ff-chain
// For the first time it is 0
const lpToMint =
  wimsReserve === 0n
    ? adaReserve + adaAmount
    : (lpTokensNumber * adaAmount) / adaReserve;
console.log("tokens to mint (lptokens is ", lpToMint);
let pkhHex =
  lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;
console.log({ pkhHex });
// Build transaction
const redeemer = new Constr(0,[
    adaAmount,
    wimsAmount,
    adaReserve,
    wimsReserve,
    lpTokensNumber,
    pkhHex,
    LPASSET_ID,
    toHex(LPTOKENName),
  ]);


const scriptUtxos = await lucid.utxosAt(validatorAddress);
// Filtrer les UTxOs du wallet
const userUtxos = (await lucid.wallet.getUtxos()).filter(
  (utxo) =>
    (utxo.assets.lovelace ?? 0n) >= adaAmount &&
    (utxo.assets[WIMSASSET_ID] ?? 0n) >= wimsAmount,
);
//const userUtxos = await lucid.utxosAt(walletAddress);

console.log("Filtered user UTxOs:", userUtxos);
console.log("scriptUtxos ", scriptUtxos);

const tx = await lucid
  .newTx()
  .collectFrom([scriptUtxos[scriptUtxos.length - 1]], redeemer) // <-- redeemer ici
  .collectFrom(userUtxos, Data.void())
  .payToContract(
    validatorAddress,
    { inline: 1n },
    {
      lovelace: adaAmount,
      [WIMSASSET_ID]: wimsAmount,
    },
  )
  .mintAssets({ [LPASSET_ID]: lpToMint }, Data.void())
  .attachMintingPolicy(mintingPolicy)
  .payToAddress(walletAddress, {
    [LPASSET_ID]: lpToMint,
  })
  .attachSpendingValidator(validator)
  .addSigner(walletAddress)
  //.addSignerKey(pkhHex)
  .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log("Tx hash:", txHash);
