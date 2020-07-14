const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const { directory } = require('./directory.js');
const { pat } = require('./string-generator.js');

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('Directory', () => {
    /**
     * I sort of knew where this test was going, but then I got lost
     * It was based on the idea that we should be able to find and nip in the bud any potentially non-terminating definitions.
     * To be clear, having the possibility of randomly choosing non-terminating paths is okay
     *
     * Test case:
     * const a = ["a", "a${b}"]
     * const b = ["b${a}"]
     *
     * Result:
     * Should be considered valid. Even though it could just choose the recursive choices over and over again, there's a possibility of it choosing a terminating path
     *
     * Test case:
     * const a = ["a${b}"]
     * const b = ["b${a}"]
     *
     * Result:
     * Should be found and marked invalid. This generator can clearly not terminate
     *
     */
    /**
    it('should be able to terminate', () => {
        const hasTerminatingPaths = {};

        function canTerminate(key, hasBeenVisited) {
            if (hasTerminatingPaths[key]) {
                return true;
            }
            for (const path of directory[key]) {
                if (!path.match(pat)) {
                    hasTerminatingPath[key] = true;
                    return true;
                }
            }
        }


        for (const [key, paths] of Object.entries(directory)) {
            const hasBeenVisited = {key: true};
            for (const path of paths) {
                if (!path.match(pat)) {
                    hasTerminatingPaths[key] = true;
                } else {
                    // Turn into an array to use all/any?
                    const matches = [...path.matchAll(pat)].map(x => x[1]);
                    if allTermin
                }
            }
        }
    });
    **/

    it('should have entries for all possible substitutions', () => {
        for (const [key, paths] of Object.entries(directory)) {
            for (const path of paths) {
                const matches = [...path.matchAll(pat)].map(x => x[1]);
                for (const match of matches) {
                    directory.should.have.property(match);
                }
            }
        }
    });
});
