const { PatternMatchable } = require('./pattern.js');

class Option extends PatternMatchable {
    constructor() {
        super([Some, None]);
    }

    static Some(value) {
        return new Some(value);
    }

    static None() {
        return new None();
    }
}

class Some extends Option {
    constructor(value) {
        super();
        this.value = value;
        this.onMatch = (f) => f(this.value);
    }
}

class None extends Option {
    constructor() {
        super();
        this.onMatch = (f) => f()
    }
}

module.exports = { Option };