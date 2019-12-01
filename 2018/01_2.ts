const frequencyChanges = require("./01_input.json");
import { loopInfinitely, lazyReduce } from "./utils";

function firstDuplicate<T>(iterable : Iterable<T>) : T | undefined {
  let sofar = new Set();
  for (let value of iterable) {
    let size = sofar.size;
    sofar.add(value);
    if (size + 1 !== sofar.size) return value;
  }
  return;
}

console.log(
  firstDuplicate(
    lazyReduce((a : number, b : number) => a + b)(0)(
      loopInfinitely(Object.values(frequencyChanges)) as Iterable<number>
    )
  )
);
