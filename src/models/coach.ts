import { processConfigValue } from "./utils/config-reader";
import * as stringUtils from '@utils/stringUtils';

export class Coach {
    idRaw: string;
    id: string;
    name: string;
    teamName: string;
    teamType: string;
    nickname: string;

    static null(): Coach {
        return new Coach({
            id: "unknown",
            name: "unknown",
            teamName: "unknown",
            teamType: "unknown",
            nickname: "unknown",
        });
    }

    constructor(data: any) {
        this.idRaw = data.id;
        this.id = processConfigValue(this.idRaw).on({
           Left: (v: string) => v,
           Right: (e: Error) => {throw e;}
        });
        this.name = data.name;
        this.teamName = data.teamName;
        this.teamType = data.teamType;
        this.nickname = data.nickname || null;
    }

    get commonName(): string {
        if (this.nickname) {
            return this.nickname;
        } else {
            return this.name;
        }
    }

    get mentionString(): string {
        return `<@${this.id}>`;
    }

    teamNameIsCloseEnough(s: string): boolean {
        return stringUtils.getSimilarity(this.teamName, s) > 0.75;
    }

    encode(): any {
        const { idRaw, name, teamName, teamType, nickname } = this;
        const id = idRaw;
        return { id, name, teamName, teamType, nickname };
    }
}
