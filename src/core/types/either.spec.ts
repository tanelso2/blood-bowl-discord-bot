import { Either } from './either';

describe('Either', () => {
    describe('on()', () => {
        it('should fail if given more than 2 args', (done) => {
            const testVal = Either.Left('Hello');
            try {
                testVal.on({
                    Left: (pat1) => {},
                    Right: (_) => {},
                    Up: (_) => {},
                });
            } catch(e) {
                done();
            }
            throw new Error("Should be unreachable");
        });

        it('should fail if given less than 2 args', (done) => {
            const testVal = Either.Left('Hello');
            try {
                testVal.on({
                    Left: (pat1) => {},
                });
            } catch(e) {
                done();
            }
            throw new Error("Should be unreachable");
        });

        it('Should do left branch on left', () => {
            const testVal = 'hello';
            const testEither = Either.Left(testVal);
            testEither.on({
                Left: (v) => v.should.equal(testVal),
                _: () => {throw new Error("Should be unreachable");}
            })
        });

        it('Should do right branch on right', () => {
            const testVal = 'hello';
            const testEither = Either.Right(testVal);
            testEither.on({
                Right: (v) => v.should.equal(testVal),
                _: () => {throw new Error("Should be unreachable");}
            });
        });

        it('Should do _ branch if no better match is found', (done) => {
            const testEither = Either.Left(undefined);
            testEither.on({
                Right: () => {throw new Error("Should be unreachable");},
                _: () => Either.Right(undefined).on({
                    Left: () => {throw new Error("Should be unreachable");},
                    _: () => Either.Right(undefined).on({
                        _: () => Either.Left(undefined).on({
                            _: () => done()
                        }),
                    }),
                }),
            });
        });
    });
});
