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
});
