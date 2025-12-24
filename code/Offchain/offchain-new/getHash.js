import { createHash } from "crypto";

// Convertit un entier en bytes BigEndian comme Plutus (sans longueur fixe)
function integerToByteString(n) {
  if (n < 0n) throw new Error("Negative not supported");
  if (n === 0n) return new Uint8Array([]);
  let hex = n.toString(16);
  if (hex.length % 2) hex = "0" + hex;
  return Uint8Array.from(hex.match(/.{1,2}/g).map((b) => parseInt(b, 16)));
}

// SHA-256 d’un Uint8Array → retourne Uint8Array
function sha2_256(bytes) {
  return createHash("sha256").update(Buffer.from(bytes)).digest();
}

// Concatène deux Uint8Array
function appendByteString(a, b) {
  const r = new Uint8Array(a.length + b.length);
  r.set(a, 0);
  r.set(b, a.length);
  return r;
}

// Reproduit getHash :: Integer -> Integer -> BuiltinByteString
export function getHash(adaR, tokR) {
  const adaBytes = integerToByteString(adaR);
  const tokBytes = integerToByteString(tokR);

  const adaHash = sha2_256(adaBytes);
  const tokHash = sha2_256(tokBytes);

  const combined = appendByteString(adaHash, tokHash);
  const finalHash = sha2_256(combined);

  return Buffer.from(finalHash).toString("hex");
}

// Exemple
// const adaR = 5000000n;
// const tokR = 10000000n;

// console.log("finalHash =", getHash(adaR, tokR));
