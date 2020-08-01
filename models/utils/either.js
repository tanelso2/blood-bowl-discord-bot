

const Either = {
    Left: makeLeft,
    Right: makeRight
};

function makeLeft(value) {
    return new Left(value);
}

function makeRight(value) {
    return new Right(value);
}

class Left {
    constructor(value) {
        this.value = value;
    }

    onLeft(f) {
        f(this.value);
    }

    onRight(_) {
        return;
    }
}

class Right {
    constructor(value) {
        this.value = value;
    }

    onLeft(_) {
        return;
    }

    onRight(f) {
        f(this.value);
    }
}

module.exports = { Either };