import { reduce } from "ramda";
import { Function as F, Tuple as T } from "ts-toolbelt";

/**
 * @param predicate
 * @param iterable
 */
export const countIf = <T>(predicate : F.Function<[T], boolean>) => (iterable : T[]) => reduce(
  (acc : number, cur : T) => (predicate(cur) ? acc + 1 : acc), 0, iterable
);

/**
 * @param index
 * @param iterable
 */
export const dropIndex = <T>(index : number) => (iterable : Iterable<T>) => {
  const arr = [...iterable];
  arr.splice(index, 1);
  return arr;
};

/**
 * @param trans
 * @param iterable
 */
export const lazyMap = <A, B>(trans : F.Function<[A], B>) => function*(iterable : Iterable<A>) {
  const bs = [];
  for (const a of iterable) {
    const b = trans(a);
    yield b;
    bs.push(b);
  }
  return bs;
};

/** A generator variant of reduce, yielding successive values of the accumulator.
 * @param reducer
 * @param initial
 * @param iterable
 */
export const lazyReduce = <A, B>(reducer : F.Function<[A, B], A>) => (initial : A) => function*(iterable : Iterable<B>) {
  let accumulator = initial;
  for (const current of iterable) {
    accumulator = reducer(accumulator, current);
    yield accumulator;
  }
  return accumulator;
}

/** A generator which provides an unending iterable by looping the given iterable.
 * @param iterable The iterable to loop
 */
export const loopInfinitely = function* <T>(iterable : Iterable<T>) {
  while (true) for (let value of iterable) yield value;
};

/**
 * @param start
 * @param end
 */
export const lazyRange = (start : number) => function*(end : number) {
  if (start === Infinity) start = Number.MAX_SAFE_INTEGER;
  else if (start === -Infinity) start = Number.MIN_SAFE_INTEGER;

  if (end === Infinity) end = Number.MAX_SAFE_INTEGER;
  else if (end === -Infinity) end = Number.MIN_SAFE_INTEGER;

  for (let i = start; i < end; i++) yield i;
}

export const range = (start : number) => (end : number) => [...lazyRange(start)(end)];

export const lazyReverse = function* <T>(array : readonly T[]) {
  for (let i = array.length - 1; i >= 0; i--) {
    yield array[i];
  }
};

/**
 * @param iterable 
 */
export const uniqueItems = <T>(iterable : Iterable<T>) => [...new Set([...iterable])];

function _multidimensionalArray<F>(dimensions : [number], fill : F) : F[];
function _multidimensionalArray<F>(dimensions : [number, number], fill : F) : F[][];
function _multidimensionalArray<F>(dimensions : [number, number, number], fill : F) : F[][][];
function _multidimensionalArray<F>(dimensions : [number, number, number, number], fill : F) : F[][][][];
function _multidimensionalArray<F>(dimensions : T.Tuple<number>, fill : F) {
  let array;
  for (const size of lazyReverse(dimensions)) {
    if (array) {
      array = Array(size).fill(array);
    } else {
      array = Array(size).fill(fill);
    }
  }
  return array;
}

// @ts-ignore
export const multidimensionalArray = <F>(dimensions : T.Tuple<number>) => (fill : F) => _multidimensionalArray(dimensions, fill);