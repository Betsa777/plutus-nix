import { Constr, Data } from "lucid-cardano";
import {
  lucid,
  walletAddress,
  validatorAddress,
  validator,
} from "./common.mjs";

const scriptUtxos = await lucid.utxosAt(validatorAddress);
const userUtxos = await lucid.wallet.getUtxos(); // Use wallet UTxOs instead of address

console.log("Script UTxOs count:", scriptUtxos.length);

if (scriptUtxos.length === 0) {
  console.log("No UTxOs found at validator address - need to initialize first");
  process.exit(1);
}

// For your validator: Integer (datum) -> Integer (redeemer) -> ScriptContext -> Bool
// You need to provide both datum and redeemer as integers
const datumValue = 42n; // Example datum
const redeemerValue = 42n; // Example redeemer - they must be equal to pass

try {
  const tx = await lucid
    .newTx()
    .collectFrom(
      scriptUtxos,
      Data.to(redeemerValue), // Simple integer redeemer
    )
    .collectFrom(userUtxos, Data.void())
    .payToContract(
      validatorAddress,
      { inline: Data.to(datumValue) }, // Simple integer datum
      { lovelace: 2_000_000n },
    )
    .attachSpendingValidator(validator)
    .addSigner(walletAddress)
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  console.log("Tx hash:", txHash);
} catch (error) {
  console.error("Transaction error:", error);
}
