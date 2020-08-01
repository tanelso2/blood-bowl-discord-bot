const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const { generateString } = require('./string-generator.js');

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('generateString()', () => {
    it('Should return something different', () => {
        const test = "${insult}";
        const result = generateString(test);
        result.should.not.equal(test);
    });
})
