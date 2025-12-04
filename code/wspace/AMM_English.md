# 🧮 AMM Smart Contract — Technical Documentation

## 📘 Overview
This smart contract implements an **Automated Market Maker (AMM)** enabling:
- decentralized swaps between **ADA** and **custom tokens**,
- **liquidity provision**,
- and **liquidity withdrawal**.

---

## 📑 Table of Contents
- [⚙️ Core Functionalities](#️-core-functionalities)
  - [➕ Add Liquidity](#-add-liquidity)
  - [🔄 Swap](#-swap)
  - [➖ Remove Liquidity](#-remove-liquidity)
- [🧠 Swap Mechanism](#-swap-mechanism)
- [💠 LP Token Calculation](#-lp-token-calculation)
- [✔️ Validation Rules](#️-validation-rules)
- [📦 Data Structures](#-data-structures)
- [🔐 Security Considerations](#-security-considerations)
- [🚀 Usage](#-usage)
- [⚠️ Limitations](#️-limitations)

---

## ⚙️ Core Functionalities

### ➕ Add Liquidity
Users can supply liquidity by depositing both ADA and tokens.

**Parameters:**
- `adaAmount` — Amount of ADA deposited  
- `tokenAmount` — Amount of tokens deposited  
- `adaR` — Current ADA reserve  
- `tokR` — Current token reserve  
- `lpTokNumb` — Total supply of LP tokens  
- `lpPKH` — Public key hash of the liquidity provider  
- `lpCurrSymb` — Currency symbol of the LP token  
- `lpTokName` — Name of the LP token  

---

### 🔄 Swap
Users can swap ADA → token or token → ADA.

**Parameters:**
- `amount` — Input amount  
- `minAmount` — Minimum expected output amount  
- `adaR` — ADA reserve  
- `tokR` — Token reserve  
- `tokSymbolIn` — Input token symbol  
- `tokSymbolOut` — Output token symbol  

---

### ➖ Remove Liquidity
Users burn their LP tokens to retrieve their proportional share of the pool.

**Parameters:**
- `amount` — LP tokens to burn  
- `adaR` — ADA reserve  
- `tokR` — Token reserve  
- `lpTokNumb` — Total LP token supply  
- `lpTokSymb`, `lpTokName` — LP token identifiers  
- `tokSymb`, `tokName` — Pool token identifiers  

---

## 🧠 Swap Mechanism

### 🔢 Pricing Formula — *Constant Product (x * y = k)*  
The contract uses the constant product invariant with **1% fee**:

```haskell
getAmountForSwp :: Amount -> Amount -> Amount -> Amount
getAmountForSwp inputAmount inputReserve outputReserve =
  let inputAmountWithFee = inputAmount * 99
      numerator = inputAmountWithFee * outputReserve
      denominator = (inputReserve * 100) + inputAmountWithFee
   in divide numerator denominator
💠 LP Token Calculation
When adding liquidity:
🆕 Pool is empty:

txt
Copier le code
tokensForLp = adaR + adaAmount
🏦 Pool already exists:

txt
Copier le code
tokensForLp = (lpTokNumb * adaAmount) / adaR
✔️ Validation Rules
Add Liquidity
All deposited amounts must be positive

The pool must receive the exact ADA/token quantities

The user must receive the correct amount of LP tokens

Swap
Input amount must be provided

Output amount must be ≥ minAmount

Pool reserves must update correctly

Remove Liquidity
LP token amount must be positive

LP tokens must be burned

ADA and tokens must be returned proportionally

Reserves must update accordingly

📦 Data Structures

data AMMRedeemer
  = AddLiquidity Amount Amount AdaReserve TokenReserve LPTokenNumber LPPKH LPTokenCurrencySymbol LPTokenName
  | Swap Amount Amount AdaReserve TokenReserve TokenSymbol TokenSymbol
  | RemoveLiquidity Amount AdaReserve TokenReserve LPTokenNumber LPTokenCurrencySymbol LPTokenName TokenSymbol TokenName
🔐 Security Considerations
Critical checks:

Reject zero or negative amounts

Signature verification

Strict output validation

Safe arithmetic operations

minAmount prevents front-running attacks

🚀 Usage
The AMM is compiled into a Plutus V2 validator and deployed on the Cardano blockchain.
Users interact through transactions containing:

the correct redeemer (AddLiquidity / Swap / RemoveLiquidity),

appropriate datum and context values.

⚠️ Limitations
Only supports ADA/token pairs

Fixed 1% fee

Liquidity reserves must be managed off-chain
