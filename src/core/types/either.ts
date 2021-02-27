import { PatternMatchable } from './pattern';

export class Either<a,b> extends PatternMatchable {
    constructor() {
        super([Left, Right]);
    }

    static Left<a,b>(value: a): Either<a,b> {
        return new Left(value);
    }

    static Right<a,b>(value: b): Either<a,b> {
        return new Right(value);
    }
}

class Left<a,b> extends Either<a,b> {
    value: a;
    constructor(value: a) {
        super();
        this.value = value;
        this.onMatch = (f: (x: a) => any) => f(this.value);
    }
}

class Right<a,b> extends Either<a,b> {
    value: b;
    constructor(value: b) {
        super();
        this.value = value;
        this.onMatch = (f: (x: b) => any) => f(this.value);
    }
}
