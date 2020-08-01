class PatternMatchable {
    constructor(n) {
        this.n = n;
    }

    static on(ref, args) {
        if (args.length !== ref.n) {
            throw new Error(`Pattern match not exhaustive! Found ${args.length} cases but need ${ref.n}`)
        }
    }
}

module.exports = { PatternMatchable };