import { PatternMatchable } from './pattern';

export class Option<a> extends PatternMatchable {
    constructor() {
        super([Some, None]);
    }

    static Some<a>(value: a) {
        return new Some(value);
    }

    static None() {
        return new None();
    }
}

class Some<a> extends Option<a> {
    value: a;
    constructor(value: a) {
        super();
        this.value = value;
        this.onMatch = (f: (x: a) => any) => f(this.value);
    }
}

class None<_> extends Option<_> {
    constructor() {
        super();
        this.onMatch = (f: () => any) => f()
    }
}
