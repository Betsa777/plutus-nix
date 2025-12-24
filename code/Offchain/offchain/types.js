import { Data, toHex } from "lucid-cardano";

// Définition du type PoolParams

export const PoolParams = Data.Object({
  adaR: Data.Integer(),
  tokR: Data.Integer(),
  tokSymbol: Data.Bytes(),
  lpTokNumb: Data.Integer(),
  lpPKH: Data.Bytes(),
  lpCurrSymb: Data.Bytes(),
  lpTokName: Data.Bytes(),
});

// Définition du type AMMRedeemer
// Chaque constructeur est un variant (sum type)
export const AMMRedeemer = Data.Enum([
  Data.Object({
    AddLiquidity: Data.Tuple([
      Data.Integer(), // Amount
      Data.Integer(), // Amount
      PoolParams, // PoolParams
    ]),
  }),
  Data.Object({
    Swap: Data.Tuple([
      Data.Integer(), // amountIn
      Data.Integer(), // amountOut
      Data.Integer(), // adaReserve
      Data.Integer(), // tokenReserve
      Data.Bytes(), // lpTokenCurrencySymbol
    ]),
  }),
  Data.Object({
    RemoveLiquidity: Data.Tuple([
      Data.Integer(), // Amount
      Data.Bytes(), // PubKeyHash
    ]),
  }),
]);
