const input : string[] = require("./02_input.json");
import { dropIndex, range } from "./utils";

root: for (const i of range(0)(Infinity)) {
  const dropI = dropIndex(i);
  for (const a of input) {
    const aDropI = dropI(a).join("");
    for (const b of input) {
      const bDropI = dropI(b).join("");
      if (aDropI === bDropI && a !== b) {
        console.log(aDropI);
        break root;
      }
    }
  }
}
