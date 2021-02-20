const { PatternMatchable } = require('./pattern.js');

export class Either<a,b> extends PatternMatchable {
    constructor() {
        super([Left, Right]);
    }

    static Left<a>(value: a) {
        return new Left(value);
    }

    static Right<b>(value: b) {
        return new Right(value);
    }
}

class Left<a,b> extends Either<a,b> {
    constructor(value: a) {
        super();
        this.value = value;
        this.onMatch = (f: (x: a) => any) => f(this.value);
    }
}

class Right<a,b> extends Either<a,b> {
    constructor(value: b) {
        super();
        this.value = value;
        this.onMatch = (f: (x: b) => any) => f(this.value);
    }
}
