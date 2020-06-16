class Coach {
    constructor(data) {
        this.id = data.id;
        this.name = data.name;
        this.teamName = data.teamName;
        this.teamType = data.teamType;
        this.nickname = data.nickname;
    }
    
    get commonName() {
        if (this.nickname) {
            return this.nickname;
        } else {
            return this.name;
        }
    }
    
    get mentionString() {
        return `<@${this.id}>`;
    }
}
