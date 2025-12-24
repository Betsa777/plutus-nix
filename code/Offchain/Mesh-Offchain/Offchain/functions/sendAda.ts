import { getTxBuilder, owner_wallet } from "./common";
const userAddress =
  "addr_test1qpy8xrm2l584g55rdsm4mj5lz663g7aldg5z4k4m8zqpg9ls0cw2c3sglqnz2kg3xpeat5d2nfw0rrt6nd83rm3px98qh637fd";
export async function sendAdaToUser() {
  const assets = [
    {
      unit: "lovelace",
      quantity: "100000000",
    },
  ];
  const txBuilder = await getTxBuilder();
  const utxos = await owner_wallet.getUtxos();
  if (!owner_wallet.addresses.baseAddressBech32)
    throw new Error("owner wallet");
  const tx = await txBuilder
    .txOut(userAddress, assets)
    .changeAddress(owner_wallet.addresses.baseAddressBech32)
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
