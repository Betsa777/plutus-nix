# NFTMarketPlace – Smart Contract Documentation

## 📘 Introduction

This smart contract implements a decentralized NFT marketplace on **Cardano (Plutus V2)**.
It supports the following actions:

* **`Sell`** — List an NFT for sale
* **`Update`** — Update the sale price
* **`Cancel`** — Cancel the sale
* **`Buy`** — Purchase the NFT

All validations are performed **on-chain**.

---

## 📦 On-Chain Types

### `MDatum`

```haskell
data MDatum = MDatum {
    price  :: Integer,
    nft    :: Value,
    seller :: PubKeyHash
}
```

### `MRedeemer`

```haskell
data MRedeemer
  = Sell Integer Value PubKeyHash
  | Buy PubKeyHash
  | Update Integer
  | Cancel
```

### Redeemer Description

| Redeemer   | Description               |
| ---------- | ------------------------- |
| **Sell**   | Initial NFT listing       |
| **Buy**    | Purchase of the NFT       |
| **Update** | Modification of the price |
| **Cancel** | Cancellation of the sale  |

---

## 🛠️ Validation – `mValidator`

The validator enforces the marketplace logic by validating **four actions**.

---

## 🟧 1. Action **Sell**

### ✔️ Conditions

* A **single output** must return to the script.
* The datum of that output must **exactly match** the fields from the redeemer:

  * `price`
  * `nft`
  * `seller`
* The output must contain **exactly 1 NFT**.
* The **seller must sign** the transaction.

---

## 🟦 2. Action **Update**

### ✔️ Conditions

* The unique script output must contain:

  * The **same NFT**
  * The **same seller**
  * The **new price**
* The seller must sign the transaction.
* Strict verification of the NFT’s `CurrencySymbol` and `TokenName`.

---

## 🟥 3. Action **Cancel**

### ✔️ Conditions

* **No output** must return to the script:

```haskell
getContinuingOutputs == []
```

* The seller must sign the transaction.
* The seller must **receive the NFT back** in their outputs.
* The returned NFT must **exactly match** the NFT stored in the datum.

---

## 🟩 4. Action **Buy**

### ✔️ Conditions

#### 🔹 Buyer Input

* The buyer must sign the transaction (`txSignedBy`).
* The buyer must provide **enough ADA** to cover the price.
* At least one input must come from the buyer’s own address and contain sufficient ADA.

#### 🔹 Buyer Output

* The buyer must receive **exactly 1 NFT** matching the datum.

#### 🔹 Seller Output

* The seller must receive **at least `price` ADA**.

#### 🔹 Script Output

* No output must remain at the script address:

```haskell
getContinuingOutputs == []
```

---

## 🔧 Utility Function: `getNftData`

```haskell
getNftData :: Value -> [(CurrencySymbol, TokenName)]
```

Returns a list of all non-ADA tokens present **with quantity = 1**.

---

## 🏗️ Compilation / Export

The script is compiled into a `.plutus` file using:

```haskell
getCbor :: IO ()
```

This generates:

```bash
./assets/marketplace.plutus
```

---

## 🔐 Guarantee Summary

| Action     | Guarantees                                          |
| ---------- | --------------------------------------------------- |
| **Sell**   | Correct NFT listing and price                       |
| **Update** | Only the seller can update the price                |
| **Cancel** | The seller retrieves the NFT and closes the listing |
| **Buy**    | Atomic swap: NFT → buyer, ADA → seller              |

---

## 🛡️ Security

* Signature verification (`txSignedBy`)
* Strict NFT validation (CurrencySymbol + TokenName)
* Prevents NFT or ADA misdirection
* No leftover script outputs for `Buy` and `Cancel`
