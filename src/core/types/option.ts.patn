pat Option<a>(  
    Some a
    | None 
)
{|
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
        return this.match({
            Some: (x) => x,
            None: () => {throw new Error("unwrap() called on None!");}
        });
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
|}