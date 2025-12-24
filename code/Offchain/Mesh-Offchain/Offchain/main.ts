import { mConStr0 } from "@meshsdk/core";
import "dotenv/config";
import {
  assets,
  createCollateral,
  createUserCollateral,
  getTxBuilder,
  getUtxosAtScript,
  owner_wallet,
  scriptAddr,
} from "./functions/common";
import { addLiquidity } from "./functions/addLiquidity";
import { createFirstUtxo } from "./functions/createFirstUtxo";
import { swapAdaToWimsToken } from "./functions/swapAda";
import { sendAdaToUser } from "./functions/sendAda";
import { swapWimsTokenToAda } from "./functions/swapToken";
import { removeLiquidity } from "./functions/removeLiquidity";
import { addLiquidityUser } from "./functions/addLiquidity_user";

async function main() {
  //Decommentez les lignes que vous souhaitez pour l'interaction puis recommentez les
  //Si vous n'avez pas de collateral decommentez la ligne await createCollateral()

  const utxo = await getUtxosAtScript();
  //if (!utxo) throw new Error("undefined utxo");
  const amount = 5_000_000n;
  //await createFirstUtxo();
  // await addLiquidity(utxo);
  await addLiquidityUser(utxo);
  //await swapAdaToWimsToken(utxo);
  //await swapWimsTokenToAda(utxo);
  //await removeLiquidity(utxo);
  //await createCollateral();
  //await sendAdaToUser();
  //await createUserCollateral();
}

main();
