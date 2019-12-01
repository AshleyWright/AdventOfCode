const input: string[] = require("./02_input.json");
import { uniqueItems, countIf } from "./utils";
import { curry, equals } from "ramda";

const containsExactly = curry(function(n: number, str: string): boolean {
  const arr = [...str];
  const uniques = uniqueItems(arr);
  for (const unique of Object.values(uniques)) {
    if (countIf(equals(unique as any), [...str]) === n) return true;
  }
  return false;
});

const countOfExactly2 = countIf(containsExactly(2), input);
const countOfExactly3 = countIf(containsExactly(3), input);

console.log(countOfExactly2 * countOfExactly3);
