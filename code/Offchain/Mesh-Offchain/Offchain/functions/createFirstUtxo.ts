import { mConStr0 } from "@meshsdk/common/dist";
import { assets, getTxBuilder, owner_wallet, scriptAddr } from "./common";

//Soumettre une première tx avec le datum initial à l'adresse du script
export async function createFirstUtxo() {
  const txBuilder = await getTxBuilder();
  const utxos = await owner_wallet.getUtxos();
  console.log({ utxos });

  if (!owner_wallet.addresses.baseAddressBech32) throw new Error("address");
  const tx = await txBuilder
    .txOut(scriptAddr, assets)
    .txOutInlineDatumValue(1n)
    .changeAddress(owner_wallet?.addresses?.baseAddressBech32)
    .selectUtxosFrom(utxos)
    .complete();

  const txSigned = await owner_wallet.signTx(tx);
  try {
    const txHash = await owner_wallet.submitTx(txSigned);
    console.log({ txHash });
  } catch (error) {
    console.log(error);
  }
}
//2536559d5286e8cc8a037c288093404ecef3336fa72c75d2e17fed22e147d572
