/* eslint-disable @typescript-eslint/no-explicit-any */
import { ErrorList } from '@utils/errorList';

const ANY = "_";

interface PatternDoing<a> {
    [x: string]: (...args: any[]) => a
}

export class PatternMatchable {
    private ctrs: any[];
    private options: string[];
    private T: string;

    constructor(ctrs: any[]) {
        this.ctrs = ctrs;
        this.options = this.ctrs.map(x => x.name);
        this.T = this.constructor.name;
    }

    _isExhaustive<a>(patterns: PatternDoing<a>): null | Error {
        const patternNames = Object.entries(patterns).map(([p, _]) => p);
        if (patternNames.includes(ANY)) {
            // wildcard is always completely exhaustive
            return null;
        }

        const failures = this.options.filter(opt => !patternNames.includes(opt));
        if (failures.length === 0) {
            return null;
        }

        return new Error(`Could not find patterns for ${failures}`);
    }

    _containsUnknowns<a>(patterns: PatternDoing<a>): null | Error {
        const patternNames = Object.entries(patterns).map(([p, _]) => p)
        const failures = patternNames.filter(p => p !== ANY && !this.options.includes(p))
        if (failures.length === 0) {
            return null;
        }

        return new Error(`Excess patterns found: ${failures}`)
    }

    on<a>(patterns: PatternDoing<a>): a {
        const errs = new ErrorList(
            this._isExhaustive(patterns),
            this._containsUnknowns(patterns)
        );
        if (!errs.empty) {
            throw errs;
        }
        const entries = Object.entries(patterns);
        const foundMatches = entries.filter(([key, _]) => this.matches(key));
        const specificMatch = foundMatches.find(([key, _]) => key !== ANY);
        let func;
        if (specificMatch === undefined) {
            // use wildcard one
            // which should be the only one in the list
            func = foundMatches[0][1];
        } else {
            func = specificMatch[1];
        }

        return this.onMatch(func);
    }

    /**
     * @param {Function<a, b>} _
     * @return {b}
     */
    onMatch<a,b>(_: (...args: any[]) => b): b {
        throw new Error(`UNDEFINED onMatch() for ${this.T}!!!`);
    }

    matches(patternName: string): boolean  {
        return patternName === this.T || patternName === ANY;
    }
}

module.exports = { PatternMatchable };
