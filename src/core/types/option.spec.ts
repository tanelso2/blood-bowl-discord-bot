import { Option } from './option';

describe('Option', () => {
    describe('isSome()', () => {
        it('should be true on Some', () => {
            const opt = Option.Some(42);
            opt.isSome().should.be.true;
        });

        it('should be false on None', () => {
            const opt = Option.None();
            opt.isSome().should.be.false;
        });
    });

    describe('isNone()', () => {
        it('should be false on Some', () => {
            const opt = Option.Some(42);
            opt.isNone().should.be.false;
        });

        it('should be true on None', () => {
            const opt = Option.None();
            opt.isNone().should.be.true;
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
            Option.ofNullable([]).isSome().should.be.true;
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
            }
            throw new Error("Should have thrown in unwrap of None");
        });
    });

});
