import { PatternMatchable } from './pattern';

export class Result<a> extends PatternMatchable {
    constructor() {
        super([Ok, Err]);
    }

    static Ok<a>(value: a) {
        return new Ok(value);
    }

    static Err(error: Error) {
        return new Err(error);
    }
}

export class Ok<a> extends Result<a> {
    result: a;
    constructor(value: a) {
        super();
        this.result = value;
        this.onMatch = (f: (x: a) => any) => f(this.result);
    }
}

export class Err<_> extends Result<_> {
    error: Error;
    constructor(error: Error) {
        super();
        this.error = error;
        this.onMatch = (f: (x: Error) => any) => f(this.error);
    }
}
