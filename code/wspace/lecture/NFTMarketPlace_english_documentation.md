Voici la **documentation adaptée en Markdown**, exactement conforme **à ta nouvelle version du smart contract** (celle avec `CurrencySymbol` et `TokenName` séparés, et non plus `Value`).
J’ai mis à jour **toutes les sections**, y compris les types, les conditions, et le workflow de validation.

---

# 🛒 **NFTMarketPlace – Smart Contract Documentation (Updated for Current Code)**

## 📘 Introduction

This Plutus V2 smart contract implements a decentralized **NFT Marketplace on Cardano**.

It supports the following marketplace actions:

* **`Sell`** — List an NFT for sale
* **`Update`** — Update the sale price
* **`Cancel`** — Cancel the sale
* **`Buy`** — Purchase the NFT

All rules are enforced **on-chain inside the validator**.

---

# 📦 On-Chain Types

## **MDatum**

Your updated datum type is:

```haskell
data MDatum = MDatum{
    price :: Integer,
    nftCs :: CurrencySymbol,
    nftTn :: TokenName,
    seller :: PubKeyHash
}
```

### **Meaning**

| Field    | Description                   |
| -------- | ----------------------------- |
| `price`  | Price of the NFT in lovelace  |
| `nftCs`  | NFT Currency Symbol           |
| `nftTn`  | NFT Token Name                |
| `seller` | Public key hash of the seller |

---

## **MRedeemer**

Your updated redeemer type is:

```haskell
data MRedeemer
  = Sell Integer CurrencySymbol TokenName PubKeyHash
  | Buy PubKeyHash
  | Update Integer
  | Cancel
```

### **Redeemer Descriptions**

| Redeemer   | Description                   |
| ---------- | ----------------------------- |
| **Sell**   | Used to list the NFT for sale |
| **Buy**    | Buyer purchases the NFT       |
| **Update** | Seller changes the price      |
| **Cancel** | Seller cancels the listing    |

---

# 🛠️ Validation Logic (`mValidator`)

The validator enforces the sale logic for each action.

---

# 🟧 **1. Sell**

When the seller lists an NFT.

### ✔️ Required Conditions

* There must be **exactly one continuing output**.
* The output datum must **exactly match** the redeemer fields:

  * `price`
  * `nftCs`
  * `nftTn`
  * `seller`
* The output must contain **1 NFT**:

  ```haskell
  valueOf (txOutValue o) nftCs nftTn == 1
  ```
* The seller must sign the transaction.

---

# 🟦 **2. Update**

When the seller changes the price of the NFT.

### ✔️ Required Conditions

* Exactly **one output back to the script**.
* The datum of that output must contain:

  * **New `price`**
  * **Same `nftCs`**
  * **Same `nftTn`**
  * **Same `seller`**
* Only the seller can update the price:

  ```haskell
  txSignedBy info (seller mDatum)
  ```

---

# 🟥 **3. Cancel**

The seller cancels the listing and wants the NFT back.

### ✔️ Required Conditions

* No continuing script outputs:

  ```haskell
  getContinuingOutputs ctx == []
  ```
* Seller must sign.
* Seller must receive the NFT back in one of their outputs.
* NFT must match:

  * same `CurrencySymbol`
  * same `TokenName`

---

# 🟩 **4. Buy**

Atomic exchange: the buyer receives the NFT, seller receives ADA.

### ✔️ Required Conditions

### 🔹 **Buyer Input**

* Buyer must sign.
* Buyer must provide **at least `price` ADA**.
* At least one input must:

  * belong to the buyer
  * contain enough ADA

### 🔹 **Buyer Output**

* Buyer must receive exactly **1 unit of the NFT**:

  ```haskell
  valueOf v nftCs nftTn == 1
  ```

### 🔹 **Seller Output**

* Seller must receive **at least the price in ADA**.

### 🔹 **Script Output**

* After the purchase:

  ```haskell
  getContinuingOutputs ctx == 0
  ```

No listing must remain on-chain.

---

# 🔧 Utility Function

### `getNftData`

You use this to detect whether a given output contains NFTs the seller should recover:

```haskell
getNftData :: Value -> [(CurrencySymbol, TokenName)]
```

It returns all **non-ADA assets with quantity = 1**.

---

# 🏗️ Compilation / Export

The validator is compiled and exported as:

```haskell
getCbor :: IO ()
```

Output file:

```
./assets/marketplace.plutus
```

---

# 🔐 Security Guarantees

| Action     | Guarantees                                                  |
| ---------- | ----------------------------------------------------------- |
| **Sell**   | Ensures correct listing matching redeemer; seller signs     |
| **Update** | Only seller can modify price; NFT cannot change             |
| **Cancel** | NFT must return to seller; script state cleared             |
| **Buy**    | Atomic swap: NFT → buyer, ADA → seller; no leftover outputs |

---
