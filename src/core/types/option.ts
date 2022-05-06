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

    static ofNullable<a>(x: a | undefined | null | typeof NaN): Option<a> {
        if (x === null || x === undefined || isNaN(x)) {
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
