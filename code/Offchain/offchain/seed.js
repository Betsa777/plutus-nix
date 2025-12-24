import { generateSeedPhrase } from "lucid-cardano";
import fs from "fs";
const seed = generateSeedPhrase();
console.log({ seed });
fs.writeFileSync("./user_seed.txt", seed);
