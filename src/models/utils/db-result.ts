/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-unsafe-return */
import { PatternMatchable } from '@core/types/pattern';

export class DBResult<_> extends PatternMatchable {
  constructor() {
    super([Result, Unverified, DBErr]);
  }

  static Result<a>(value: a): DBResult<a> {
    return new Result(value);
  }

  static Unverified<a>(value: any): DBResult<a> {
    return new Unverified(value);
  }

  static DBErr<a>(e: Error): DBResult<a> {
    return new DBErr(e);
  }
}

class Result<a> extends DBResult<a> {
  readonly value: a;
  constructor(value: a) {
    super();
    this.value = value;
    this.onMatch = (f: (x: a) => any) => f(this.value);
  }
}

class Unverified<a> extends DBResult<a> {
  readonly value: any[];
  constructor(value: any[]) {
    super();
    this.value = value;
    this.onMatch = (f: (x: any) => any) => f(this.value);
  }
}

class DBErr<a> extends DBResult<a> {
  readonly error: Error;
  constructor(error: Error) {
    super();
    this.error = error;
    this.onMatch = (f: (x: Error) => any) => f(this.error);
  }
}