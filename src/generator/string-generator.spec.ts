import { generateString } from './string-generator';
import { Counter } from '@core/types/counter';
//import { logger } from '@core/logger';

describe('generateString()', () => {
    it('should return something different', () => {
        const test = "${insult}";
        const result = generateString(test);
        result.should.not.equal(test);
    });

    describe('Regressions', () => {
        it("should not replace all fields", () => {
            const test = "${dumbSynonym} you put your ${clothing} on ${bodyPart}";
            const result = generateString(test);
            const wordCount: Counter = {};
            result.split(/\s/).forEach((word: string) => {
                wordCount[word] = wordCount[word] ? wordCount[word] + 1 : 1;
                if (wordCount[word] > 2) {
                    throw new Error(`repeated word ${word}`);
                }
            });
        });
    });
})
