import { lucid, walletAddress } from "../common.mjs";
import { fromText } from "lucid-cardano"; // ✅ Remplace stringToHex()

const { paymentCredential } = lucid.utils.getAddressDetails(walletAddress);

export const mintingPolicy = lucid.utils.nativeScriptFromJson({
  type: "sig",
  keyHash: paymentCredential.hash,
});

const policyId = lucid.utils.mintingPolicyToId(mintingPolicy);

const tokenA = { assetName: "WIMSTOKEN", amount: 10_000_000n };
const tokenB = { assetName: "LPTOKEN", amount: 10_000_000n };

console.log("Policy ID:", policyId);

// 🪙 Conversion du nom de token en hex avec fromText()
const assetA = policyId + fromText(tokenA.assetName);
const assetB = policyId + fromText(tokenB.assetName);

// --- Création du minting policy ---
async function mint() {
  const tx = await lucid
    .newTx()
    .mintAssets(
      {
        [assetA]: tokenA.amount,
        [assetB]: tokenB.amount,
      },
      undefined,
    )
    .attachMintingPolicy(mintingPolicy)
    .payToAddress(walletAddress, {
      lovelace: 2_000_000n,
      [assetA]: tokenA.amount,
      [assetB]: tokenB.amount,
    })
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  console.log("✅ Tokens minted!");
  console.log("Tx hash:", txHash);
  console.log("Policy ID:", policyId);
  console.log("Assets:", {
    WIMSTOKEN: assetA,
    LPTOKEN: assetB,
  });
}

//mint();
export const POLICY_ID =
  "ea055095c321927041cc6baa0076efeef9f3b60a21b1998b9da79c67";
export const WIMSTOKENName = "WIMSTOKEN";
export const LPTOKENName = "LPTOKEN";
export const WIMSASSET_ID = assetA;
export const LPASSET_ID = assetB;
// Policy ID: ea055095c321927041cc6baa0076efeef9f3b60a21b1998b9da79c67
// ✅ Tokens minted!
// Tx hash: 778c0ac50f8d71174c573e5adfb4fecdb9c74e93212e1bb5b6c05db52637531a
// Policy ID: ea055095c321927041cc6baa0076efeef9f3b60a21b1998b9da79c67
// Assets: {
//   WIMSTOKEN: 'ea055095c321927041cc6baa0076efeef9f3b60a21b1998b9da79c6757494d53544f4b454e',
//   LPTOKEN: 'ea055095c321927041cc6baa0076efeef9f3b60a21b1998b9da79c674c50544f4b454e'
// }
