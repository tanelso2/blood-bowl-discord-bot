/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-unsafe-return */
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

    static ofNullable<a>(x: a | undefined | null | Option<a>): Option<a> {
        if (x instanceof Option<a>) {
            return x;
        }
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
    isSome(): this is Some<a> {
        return this instanceof Some;
    }

    /**
     * Returns if the option contains no value.
     *
     * @return {bool}
     */
    isNone(): this is None<a> {
        return this instanceof None;
    }

    map<b>(f: (x: a) => b): Option<b> {
        return this.on({
            Some: (x: a) => Option.Some(f(x)),
            None: () => Option.None()
        });
    }

    flatMap<b>(f: (x: a) => Option<b>): Option<b> {
        return this.map(x => f(x).on({
            Some: (v) => v,
            None: () => Option.None()
        }));
    }
}

class Some<a> extends Option<a> {
    readonly value: a;
    constructor(value: a) {
        super();
        this.value = value;
        this.onMatch = (f: (x: a) => any) => f(this.value);
    }

    unwrap(): a {
        return this.value;
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
}
