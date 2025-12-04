
# NFTMarketPlace – Documentation du Smart Contract

## 📘 Introduction

Ce smart contract implémente un marketplace NFT décentralisé sur Cardano (Plutus V2).  
Il permet les actions suivantes :

- `Sell` — Mise en vente d’un NFT  
- `Update` — Mise à jour du prix  
- `Cancel` — Annulation de la vente  
- `Buy` — Achat d’un NFT  

Toutes les validations sont faites **on-chain**.

---

## 📦 Types On-Chain

### ### `MDatum`

```haskell
data MDatum = MDatum {
    price  :: Integer,
    nft    :: Value,
    seller :: PubKeyHash
}
````

### `MRedeemer`

```haskell
data MRedeemer
  = Sell Integer Value PubKeyHash
  | Buy PubKeyHash
  | Update Integer
  | Cancel
```

| Redeemer | Description                   |
| -------- | ----------------------------- |
| `Sell`   | Mise en vente initiale du NFT |
| `Buy`    | Achat du NFT                  |
| `Update` | Mise à jour du prix           |
| `Cancel` | Annulation de la vente        |

---

# 🛠️ Validation – `mValidator`

Le validator implémente la logique du marketplace en validant 4 actions.

---

# 🟧 1. Action `Sell`

### ✔️ Conditions

* Un seul output retourné au script.
* Le datum de cet output doit correspondre exactement aux valeurs du redeemer :

  * `price`
  * `nft`
  * `seller`
* L’output doit contenir exactement **1 NFT**.
* Le vendeur doit signer la transaction.

---

# 🟦 2. Action `Update`

### ✔️ Conditions

* L’unique output retourné au script doit contenir :

  * le **même NFT**
  * le **même vendeur**
  * le **nouveau prix**
* Le vendeur doit signer la transaction.
* Vérification stricte du `CurrencySymbol` et `TokenName` du NFT.

---

# 🟥 3. Action `Cancel`

### ✔️ Conditions

* Aucun output ne doit retourner au script :

  ```
  getContinuingOutputs == []
  ```
* Le vendeur doit signer.
* Le vendeur doit récupérer le NFT dans ses outputs.
* Le NFT retourné doit correspondre exactement à celui du datum.

---

# 🟩 4. Action `Buy`

### ✔️ Conditions

#### **buyerInput**

* Le buyer doit signer (`txSignedBy`).
* Il doit fournir assez d’ADA pour couvrir `price`.
* Un input doit appartenir à son adresse et contenir assez d’ADA.

#### **buyerOutput**

* Le buyer doit recevoir **exactement 1 NFT** correspondant à celui dans le datum.

#### **sellerOutput**

* Le vendeur doit recevoir au moins `price` en ADA.

#### **scriptOutput**

* Le script ne doit plus avoir d’output :

  ```
  getContinuingOutputs == []
  ```

---

## 🔧 Fonction utilitaire : `getNftData`

```haskell
getNftData :: Value -> [(CurrencySymbol, TokenName)]
```

Retourne la liste des tokens **non-ADA** dont la quantité est `1`.

---

## 🏗️ Compilation / Export

Le script est compilé en fichier `.plutus` via :

```haskell
getCbor :: IO ()
```

Il génère :

```
./assets/marketplace.plutus
```

---

## 🔐 Résumé des Garanties

| Action | Garanties                                      |
| ------ | ---------------------------------------------- |
| Sell   | Publication correcte du NFT + prix             |
| Update | Seul le vendeur peut modifier le prix          |
| Cancel | Le vendeur récupère son NFT et ferme la vente  |
| Buy    | Achat atomique : NFT → acheteur, ADA → vendeur |

---

## 🛡️ Sécurité

* Vérification des signatures (`txSignedBy`)
* Vérification stricte du NFT (`CurrencySymbol`, `TokenName`)
* Empêche la déviation des ADA ou du NFT
* Aucune sortie résiduelle pour `Buy` et `Cancel`

```
