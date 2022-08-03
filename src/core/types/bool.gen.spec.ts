import { Bool } from './generated/bool';

describe('generated Bool', () => {
    describe('match()', () => {
        it('should match True', () => {
            const testVal = Bool.True();
            let y = 0;
            testVal.match({
                True: () => {
                    y = 1;
                },
                False: () => {throw new Error("Should be unreachable")} 
            })
            y.should.be.eql(1);
            const x = testVal.match({
                False: () => {throw new Error("Should be unreachable")} ,
                _: () => { return 1; }
            });
            x.should.be.eql(1);
        })

    })
});