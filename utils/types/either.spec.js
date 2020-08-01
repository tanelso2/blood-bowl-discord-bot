const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const { Either } = require('./either.js');

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('Either', () => {
    describe('Left', () => {
        const testVal = Either.Left('Hello');
        it('should run onLeft', (done) => {
            testVal.onLeft((_) => done());
        });

        it('should not run onRight', () => {
            testVal.onRight((_) => {
                throw new Error("Should not get here");
            });
        });
    });

    describe('Right', () => {
        const testVal = Either.Right('Hello');
        it('should run onRight', (done) => {
            testVal.onRight((_) => done());
        });

        it('should not run onLeft', () => {
            testVal.onLeft((_) => {
                throw new Error("Should not get here");
            });
        });

    });

    describe('on()', () => {
        it('should fail if given more than 2 args', (done) => {
            const testVal = Either.Left('Hello');
            try {
                testVal.on(
                    (pat1) => {},
                    (pat2) => {},
                    (pat3) => {}
                );
                throw new Error("Should be unreachable");
            } catch(e) {
                done();
            }
        });

        it('should fail if given less than 2 args', (done) => {
            const testVal = Either.Left('Hello');
            try {
                testVal.on(
                    (pat1) => {}
                );
                throw new Error("Should be unreachable");
            } catch(e) {
                done();
            }
        });
    });
});
