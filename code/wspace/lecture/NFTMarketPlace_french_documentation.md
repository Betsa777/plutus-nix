
---

# 🛒 **NFTMarketPlace – Documentation du Smart Contract**

## 📘 Introduction

Ce smart contract implémente un **marketplace NFT décentralisé sur Cardano (Plutus V2)**.

Il prend en charge les actions suivantes :

* **`Sell`** — Mettre un NFT en vente
* **`Update`** — Modifier le prix de vente
* **`Cancel`** — Annuler la vente
* **`Buy`** — Acheter un NFT

Toutes les règles sont **validées on-chain**.

---

## 📦 Types On-Chain

### **MDatum**

```haskell
data MDatum = MDatum{
    price  :: Integer,
    nftCs  :: CurrencySymbol,
    nftTn  :: TokenName,
    seller :: PubKeyHash
}
```

| Champ    | Description             |
| -------- | ----------------------- |
| `price`  | Prix du NFT en lovelace |
| `nftCs`  | Currency Symbol du NFT  |
| `nftTn`  | Token Name du NFT       |
| `seller` | Clé publique du vendeur |

---

### **MRedeemer**

```haskell
data MRedeemer
  = Sell Integer CurrencySymbol TokenName PubKeyHash
  | Buy PubKeyHash
  | Update Integer
  | Cancel
```

| Redeemer   | Description                           |
| ---------- | ------------------------------------- |
| **Sell**   | Mettre le NFT en vente                |
| **Buy**    | Achat du NFT                          |
| **Update** | Mise à jour du prix par le vendeur    |
| **Cancel** | Annulation de la vente par le vendeur |

---

## 🛠️ Validation – `mValidator`

Le validator applique la logique du marketplace en validant **les quatre actions**.

---

# 🟧 1. Action **Sell**

### ✔️ Conditions

* **Un seul output** doit revenir au script.

* Le datum de cet output doit **correspondre exactement** aux valeurs du redeemer :

  * `price`
  * `nftCs`
  * `nftTn`
  * `seller`

* L’output doit contenir exactement **1 NFT** :

  ```haskell
  valueOf (txOutValue o) nftCs nftTn == 1
  ```

* Le **vendeur doit signer** la transaction.

---

# 🟦 2. Action **Update**

### ✔️ Conditions

* L’unique output retourné au script doit contenir :

  * le **même NFT**
  * le **même vendeur**
  * le **nouveau prix**

* Le vendeur doit signer la transaction.

* Vérification stricte du `CurrencySymbol` et `TokenName` du NFT.

---

# 🟥 3. Action **Cancel**

### ✔️ Conditions

* **Aucun output** ne doit revenir au script :

  ```haskell
  getContinuingOutputs ctx == []
  ```
* Le vendeur doit signer la transaction.
* Le vendeur doit **récupérer le NFT** dans ses outputs.
* Le NFT retourné doit correspondre exactement au datum initial.

---

# 🟩 4. Action **Buy**

### ✔️ Conditions

#### 🔹 **Input de l’acheteur**

* L’acheteur doit signer (`txSignedBy`).
* Il doit fournir **au moins le prix en ADA**.
* Au moins un input doit provenir de son adresse et contenir assez d’ADA.

#### 🔹 **Output de l’acheteur**

* L’acheteur doit recevoir **exactement 1 NFT** correspondant au datum.

#### 🔹 **Output du vendeur**

* Le vendeur doit recevoir **au moins le prix en ADA**.

#### 🔹 **Output du script**

* Après l’achat :

  ```haskell
  getContinuingOutputs ctx == 0
  ```
* Aucune sortie résiduelle ne doit rester au script.

---

## 🔧 Fonction utilitaire : `getNftData`

```haskell
getNftData :: Value -> [(CurrencySymbol, TokenName)]
```

Retourne la liste des **tokens non-ADA** dont la quantité est **exactement 1**.

---

## 🏗️ Compilation / Export

Le validator est compilé et exporté en fichier `.plutus` via :

```haskell
getCbor :: IO ()
```

Génère le fichier :

```
./assets/marketplace.plutus
```

---

## 🔐 Résumé des Garanties

| Action     | Garanties                                        |
| ---------- | ------------------------------------------------ |
| **Sell**   | NFT publié correctement avec le bon prix         |
| **Update** | Seul le vendeur peut modifier le prix            |
| **Cancel** | Le vendeur récupère le NFT et ferme la vente     |
| **Buy**    | Échange atomique : NFT → acheteur, ADA → vendeur |

---

## 🛡️ Sécurité

* Vérification des signatures (`txSignedBy`)
* Vérification stricte du NFT (`CurrencySymbol`, `TokenName`)
* Empêche la déviation des ADA ou du NFT
* Aucune sortie résiduelle pour `Buy` et `Cancel`

---

