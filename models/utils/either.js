class Either {
    static Left(value) {
        return new Left(value);
    }

    static Right(value) {
        return new Right(value);
    }

    on(leftFunc, rightFunc) {
        this.onLeft(leftFunc);
        this.onRight(rightFunc);
    }

}

class Left extends Either {
    constructor(value) {
        super();
        this.value = value;
    }

    onLeft(f) {
        f(this.value);
        return this;
    }

    onRight(_) {
        return this;
    }
}

class Right extends Either {
    constructor(value) {
        super();
        this.value = value;
    }

    onLeft(_) {
        return this;
    }

    onRight(f) {
        f(this.value);
        return this;
    }
}

module.exports = { Either };