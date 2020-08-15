const { ErrorList } = require('../errorList');

const ANY = "_";

class PatternMatchable {

    constructor(ctrs) {
        this.ctrs = ctrs;
        this.options = this.ctrs.map(x => x.name);
        this.T = this.constructor.name;
    }

    _isExhaustive(patterns) {
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

    _containsUnknowns(patterns) {
        const patternNames = Object.entries(patterns).map(([p, _]) => p)
        const failures = patternNames.filter(p => p !== ANY && !this.options.includes(p))
        if (failures.length === 0) {
            return null;
        }

        return new Error(`Excess patterns found: ${failures}`)
    }

    on(patterns) {
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

        this.onMatch(func);
    }

    matches(patternName)  {
        return patternName === this.T || patternName === ANY;
    }
}

module.exports = { PatternMatchable };