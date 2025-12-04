# 🧮 AMM Smart Contract – Documentation Technique

## 📘 Vue d'ensemble
Ce contrat intelligent implémente un **Automated Market Maker (AMM)** permettant :
- des échanges décentralisés entre **ADA** et **tokens personnalisés**,
- la **fourniture** et le **retrait** de liquidités.

---

## 📑 Table des matières
- [⚙️ Fonctionnalités principales](#️-fonctionnalités-principales)
  - [➕ Ajout de Liquidité (AddLiquidity)](#-ajout-de-liquidité-addliquidity)
  - [🔄 Échange (Swap)](#-échange-swap)
  - [➖ Retrait de Liquidité (RemoveLiquidity)](#-retrait-de-liquidité-removeliquidity)
- [🧠 Mécanisme d’échange](#-mécanisme-déchange)
- [💠 Calcul des LP tokens](#-calcul-des-lp-tokens)
- [✔️ Conditions de validation](#️-conditions-de-validation)
- [📦 Structure des données](#-structure-des-données)
- [🔐 Sécurité](#-sécurité)
- [🚀 Utilisation](#-utilisation)
- [⚠️ Limitations](#️-limitations)

---

## ⚙️ Fonctionnalités principales

### ➕ Ajout de Liquidité *(AddLiquidity)*
L’utilisateur fournit simultanément de l’ADA et des tokens.

**Paramètres :**
- `adaAmount` — Montant d’ADA déposé  
- `tokenAmount` — Montant de tokens déposés  
- `adaR` — Réserve d’ADA actuelle  
- `tokR` — Réserve de tokens  
- `lpTokNumb` — Total de LP tokens existants  
- `lpPKH` — Clé publique du fournisseur de liquidité  
- `lpCurrSymb` — Symbole monétaire des LP tokens  
- `lpTokName` — Nom des LP tokens  

---

### 🔄 Échange *(Swap)*
Permet de swap ADA → token ou token → ADA.

**Paramètres :**
- `amount` — Montant à échanger  
- `minAmount` — Montant minimum attendu  
- `adaR` — Réserve ADA  
- `tokR` — Réserve tokens  
- `tokSymbolIn` — Token d’entrée  
- `tokSymbolOut` — Token de sortie  

---

### ➖ Retrait de Liquidité *(RemoveLiquidity)*
L’utilisateur brûle ses LP tokens pour récupérer sa part du pool.

**Paramètres :**
- `amount` — LP tokens brûlés  
- `adaR` — Réserve ADA  
- `tokR` — Réserve tokens  
- `lpTokNumb` — Total LP tokens existants  
- `lpTokSymb`, `lpTokName` — Caractéristiques des LP tokens  
- `tokSymb`, `tokName` — Token du pool  

---

## 🧠 Mécanisme d’échange

### 🔢 Formule de pricing — *Constant Product (x * y = k)*  
Avec **1% de frais** :

```haskell
getAmountForSwp :: Amount -> Amount -> Amount -> Amount
getAmountForSwp inputAmount inputReserve outputReserve =
  let inputAmountWithFee = inputAmount * 99
      numerator = inputAmountWithFee * outputReserve
      denominator = (inputReserve * 100) + inputAmountWithFee
   in divide numerator denominator
💠 Calcul des LP tokens
Lorsque l’utilisateur ajoute des liquidités :
🆕 Pool vide :

txt
Copier le code
tokensForLp = adaR + adaAmount
🏦 Pool existant :

txt
Copier le code
tokensForLp = (lpTokNumb * adaAmount) / adaR
✔️ Conditions de validation
Pour l’ajout de liquidité
Montants strictement positifs

Bonne quantité d’ADA et de tokens reçue par le pool

Attribution correcte des LP tokens

Pour l’échange
Montant d’entrée cohérent

Sortie ≥ minAmount

Mise à jour correcte des réserves

Pour le retrait de liquidité
amount positif

LP tokens correctement brûlés

Restitution proportionnelle ADA/tokens

Mise à jour des réserves

📦 Structure des données
haskell


data AMMRedeemer
  = AddLiquidity Amount Amount AdaReserve TokenReserve LPTokenNumber LPPKH LPTokenCurrencySymbol LPTokenName
  | Swap Amount Amount AdaReserve TokenReserve TokenSymbol TokenSymbol
  | RemoveLiquidity Amount AdaReserve TokenReserve LPTokenNumber LPTokenCurrencySymbol LPTokenName TokenSymbol TokenName
🔐 Sécurité
Points critiques :

Rejet des montants négatifs ou nuls

Vérification des signatures

Validation complète des outputs

Gestion prudente des divisions et multiplications

Protection contre le front-running avec minAmount

🚀 Utilisation
Le script est compilé en validateur Plutus V2, puis déployé sur Cardano.
Les utilisateurs doivent fournir :

un redeemer adapté (AddLiquidity / Swap / RemoveLiquidity),

les datums et contextes nécessaires.

⚠️ Limitations
Support actuel : uniquement ADA/token

Frais fixes 1%

Mise à jour des réserves gérées hors-chaîne
