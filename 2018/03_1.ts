const input : string[] = require("./03_input.json");
import { map, reduce, add } from "ramda";
import { lazyRange, countIf, lazyMap, multidimensionalArray } from "./utils";

const fabricArray : number[][] = multidimensionalArray([1000, 1000])(0);

const parseClaims = lazyMap((str : string) => {
  const match = /^#(?<id>\d*) @ (?<x>\d*),(?<y>\d*): (?<w>\d*)x(?<h>\d*)$/.exec(str);
  if (match && match.groups) {
    return map(v => parseInt(v, 10), match.groups as any) as Record<"id" | "x" | "y" | "w" | "h", number>;
  }
  throw new Error();
});

for (const { x, y, w, h } of parseClaims(input)) for (const δy of lazyRange(0)(h)) for (const δx of lazyRange(0)(w)) fabricArray[y + δy][x + δx]++;

console.log(reduce<number, number>(add, 0, map(countIf((n : number) => n > 1), fabricArray)));
