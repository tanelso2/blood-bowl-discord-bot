const { PatternMatchable } = require('./pattern.js');

class Option extends PatternMatchable {
    constructor() {
        super(2);
    }

    static Some(value) {
        return new Some(value);
    }

    static None() {
        return new None();
    }

    on(someFunc, noneFunc) {
        PatternMatchable.on(this, arguments);
        this.onSome(someFunc);
        this.onNone(noneFunc);
    }
}

class Some extends Option {
    constructor(value) {
        super();
        this.value = value;
    }

    onSome(f) {
        f(this.value);
        return this;
    }

    onNone(_) {
        return this;
    }
}

class None extends Option {
    constructor() {
        super();
    }

    onSome(_) {
        return this;
    }

    onNone(f) {
        f();
        return this;
    }
}

module.exports = { Option };