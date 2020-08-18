const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const { processConfigValue } = require('./config-reader.js');

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('processConfigValue', () => {
    it('Should return a number as-is', () => {
        const test = '8675309';
        processConfigValue(test).on({
            Left: (result) => result.should.equal(test),
            Right: (e) => {throw e;}
        })
    });

    it('Should return default for non-existent file w/ default', () => {
        const defaultVal = 'abcdef';
        const nonExistentFile  = 'this-should-not-exist.mp6';
        const configValue = `$f{${nonExistentFile}:${defaultVal}}`;
        processConfigValue(configValue).on({
            Left: (result) => result.should.equal(defaultVal),
            Right: (e) => {throw e;}
        });
    });

    it('Should return Error on non-existent file', (done) => {
        const nonExistentFile = 'list-of-people-that-dont-like-calvin-and-hobbes.txt';
        const configValue = `$f{${nonExistentFile}}`;
        processConfigValue(configValue).on({
            Left: () => {throw new Error("Should not reach here");},
            Right: () => done()
        });
    });
});