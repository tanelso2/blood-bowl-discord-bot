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

    describe('Regressions', () => {
        it("should not replace all fields", () => {
            const test = "${dumbSynonym} you put your ${clothing} on ${bodyPart}";
            const result = generateString(test);
            const wordCount = {};
            result.split(/\s/).forEach((word) => {
                wordCount[word] = wordCount[word] ? wordCount[word] + 1 : 1;
                if (wordCount[word] > 2) {
                    throw new Error(`repeated word ${word}`);
                }
            });
        });
    });
})
