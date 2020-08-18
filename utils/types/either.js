const { PatternMatchable } = require('./pattern.js');

class Either extends PatternMatchable {
    constructor() {
        super([Left, Right]);
    }

    static Left(value) {
        return new Left(value);
    }

    static Right(value) {
        return new Right(value);
    }
}

class Left extends Either {
    constructor(value) {
        super();
        this.value = value;
        this.onMatch = (f) => f(this.value);
    }
}

class Right extends Either {
    constructor(value) {
        super();
        this.value = value;
        this.onMatch = (f) => f(this.value);
    }
}

module.exports = { Either };