/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
import { PatternMatchable } from './pattern';

export class Option<a> extends PatternMatchable {
    constructor() {
        super([Some, None]);
    }

    static Some<a>(value: a): Option<a> {
        return new Some(value);
    }

    static None<a>(): Option<a> {
        return new None();
    }

    static ofNullable<a>(x: a | undefined | null): Option<a> {
        if (x === null || x === undefined || (typeof x === 'number' && isNaN(x))) {
            return Option.None();
        } else {
            return Option.Some(x);
        }
    }

    /**
     * Unwraps the value if Some, else if None throws.
     */
    unwrap(): a {
        throw new Error("Used a raw Option!");
    }

    /**
     * Returns if the option contains a value.
     *
     * @return {bool}
     */
    isSome(): boolean {
        throw new Error("Used a raw Option!");
    }

    /**
     * Returns if the option contains no value.
     *
     * @return {bool}
     */
    isNone(): boolean {
        throw new Error("Used a raw Option!");
    }
}

class Some<a> extends Option<a> {
    value: a;
    constructor(value: a) {
        super();
        this.value = value;
        this.onMatch = (f: (x: a) => any) => f(this.value);
    }

    unwrap(): a {
        return this.value;
    }

    isSome(): boolean {
        return true;
    }

    isNone(): boolean {
        return false;
    }
}

class None<_> extends Option<_> {
    constructor() {
        super();
        this.onMatch = (f: () => any) => f()
    }

    unwrap<_>(): _ {
        throw new Error("Unwrapped a None!");
    }

    isSome(): boolean {
        return false;
    }

    isNone(): boolean {
        return true;
    }
}
