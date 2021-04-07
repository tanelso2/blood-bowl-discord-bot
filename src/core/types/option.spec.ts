import { Option } from './option';

describe('Option', () => {
    describe('ofNullable()', () => {
        it('should transform null into None', () => {
            Option.ofNullable(null).on({
                Some: (_) => {
                    throw new Error("Should not generate Some(null)")
                },
                None: () => {},
            });
        });

        it('should transform undefined into None', () => {
            Option.ofNullable(undefined).on({
                Some: (_) => {
                    throw new Error("Should not generate Some(null)")
                },
                None: () => {},
            });
        });

        it('should transform NaN into None', () => {
            Option.ofNullable(NaN).on({
                Some: (_) => {
                    throw new Error("Should not generate Some(null)")
                },
                None: () => {},
            });
        });

        it('should transform other falsy values into Some', () => {
            Option.ofNullable(false).on({
                Some: (_) => {},
                None: () => {
                    throw new Error("Should not generate None")
                }
            });
            Option.ofNullable(0).on({
                Some: (_) => {},
                None: () => {
                    throw new Error("Should not generate None")
                }
            });
            Option.ofNullable('').on({
                Some: (_) => {},
                None: () => {
                    throw new Error("Should not generate None")
                }
            });
        });
    });

    describe('unwrap()', () => {
        it('should yield inner value from Some', () => {
            const inner = 42;
            const opt = Option.Some(inner);
            try {
                if (opt.unwrap() != inner) {
                    throw new Error(`inner value ${opt.unwrap()} of Some did not match expected inner value of ${inner}`);
                }
            } catch (e) {
                throw new Error('Should not throw in unwrap of Some');
            }
        });

        it('should throw if called on None', (done) => {
            try {
                Option.None().unwrap();
            } catch {
                done();
            } finally {
                throw new Error("Should have thrown in unwrap of None");
            }
        });
    });

    describe('isSome()', () => {
        it('should align with pattern matching', () => {
            const opt = Option.Some(42);
            if (!opt.isSome()) {
                throw new Error("Some should declare isSome()");
            } else if (opt.isNone()) {
                throw new Error("Some should not declare isNone()");
            }
        });
    });

    describe('isNone()', () => {
        it('should align with pattern matching', () => {
            const opt = Option.None();
            if (!opt.isNone()) {
                throw new Error("None should declare isNone()");
            } else if (opt.isSome()) {
                throw new Error("None should not declare isSome()");
            }
        });
    });

    describe('pattern matching', () => {
        it('should implement pattern matching', () => {
            Option.Some(42).on({
                Some: (_) => {},
                None: () => {
                    throw new Error("Some was matched to None");
                },
            });
            Option.None().on({
                Some: (_) => {
                    throw new Error("None was matched to Some");
                },
                None: () => {},
            });
        });
    });
});
