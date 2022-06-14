export type TypeGuard<a> = (x: unknown) => x is a;

export function allAre<a>(l: unknown[], t: TypeGuard<a>): l is a[] {
  return l.every(x => t(x));
}

export function isBoth<a,b>(x: unknown, ta: TypeGuard<a>, tb: TypeGuard<b>): x is a & b {
  return ta(x) && tb(x);
}
