export class ErrorList extends Error {
    readonly errors: Error[];

    constructor(...errors: (Error | null)[]) {
        super();
        this.errors = errors.filter(e => e !== null) as Error[];
        const messages = this.errors.map(e => e.message).join("\n\n")
        this.message = `ErrorList: ${messages}`;
    }

    get empty(): boolean {
        return this.errors.length === 0;
    }
}
