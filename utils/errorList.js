class ErrorList extends Error {
    constructor(...errors) {
        super();
        this.errors = errors.filter(e => e !== null);
    }

    get message() {
        const messages = this.errors.map(e => e.message).join("\n\n")
        return `ErrorList: ${messages}`;
    }

    get empty() {
        return this.errors.length === 0;
    }
}

module.exports = { ErrorList };