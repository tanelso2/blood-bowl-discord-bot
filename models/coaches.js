const { processConfigValue } = require("./utils/config-reader.js");

class Coach {
    static null() {
        return new Coach({
            id: "unknown",
            name: "unknown",
            teamName: "unknown",
            teamType: "unknown",
            nickname: "unknown",
        });
    }

    constructor(data) {
        this.idRaw = data.id;
        this.id = processConfigValue(this.idRaw).on({
           Left: (v) => v,
           Right: (e) => {throw e;}
        });
        this.name = data.name;
        this.teamName = data.teamName;
        this.teamType = data.teamType;
        this.nickname = data.nickname || null;
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

    encode() {
        const { idRaw, name, teamName, teamType, nickname } = this;
        const id = idRaw;
        return { id, name, teamName, teamType, nickname };
    }
}

module.exports = { Coach };
