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
        it('should pass through other Options', () => {
            const val = Math.random();
            const x = Option.Some(val);
            const y = Option.ofNullable(x);
            y.isSome().should.be.true;
            y.unwrap().should.eql(val);

            const x2: Option<number> = Option.None();
            const y2 = Option.ofNullable(x2);
            y2.isNone().should.be.true;
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

    describe('map()', () => {
        it('should map Nones to Nones', () => {
            const x = Option.None<number>();
            const y = x.map(x => x+1);
            y.isNone().should.be.true;
        });

        it('should map Somes properly', () => {
            const x = Option.Some(1);
            const y = x.map(x => x+1);
            y.isSome().should.be.true;
            y.unwrap().should.eql(2);

            const z = x.map(x => x*3).map(x => x+1);
            z.isSome().should.be.true;
            z.unwrap().should.eql(4);
        });

        it('should be able to change types', () => {
            const x = Option.Some(1);
            const y = x.map(x => x.toString());
            y.isSome().should.be.true;
            y.unwrap().should.eql("1");
        });
    });

    describe('flatMap()', () => {
        it('should flatten nested Options', () => {
            const x = Option.Some(1);
            const y = x.flatMap(x => Option.Some(x+1));
            y.isSome().should.be.true;
            y.unwrap().should.eql(2);
        });
    });

});
