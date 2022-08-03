import { processConfigValue } from "./utils/config-reader";
import { Option } from '@core/types/generated/option';
import * as stringUtils from '@utils/stringUtils';

export interface CoachData {
    id: string;
    name: string;
    teamName: string;
    teamType: string;
    nickname?: string | undefined;
    standing?: number | undefined;
}

export class Coach implements CoachData {
    idRaw: string;
    id: string;
    name: string;
    teamName: string;
    teamType: string;
    nickname?: string | undefined;
    nicknameOpt: Option<string>;
    standing?: number | undefined;

    static null(): Coach {
        return new Coach({
            id: "unknown",
            name: "unknown",
            teamName: "unknown",
            teamType: "unknown",
            nickname: "unknown",
        });
    }

    constructor(data: CoachData) {
        this.idRaw = data.id;
        this.id = processConfigValue(this.idRaw).on({
           Left: (v: string) => v,
           Right: (e: Error) => {throw e;}
        });
        this.name = data.name;
        this.teamName = data.teamName;
        this.teamType = data.teamType;
        this.nickname = data.nickname || undefined;
        this.nicknameOpt = Option.ofNullable(this.nickname);
        this.standing = data.standing || undefined;
    }

    get commonName(): string {
        return this.nicknameOpt.on({
            Some: (nickname: string) => nickname,
            None: () => this.name,
        });
    }

    get mentionString(): string {
        return `<@${this.id}>`;
    }

    teamNameIsCloseEnough(s: string): boolean {
        return stringUtils.getSimilarity(this.teamName, s) > 0.75;
    }

    encode(): CoachData {
        const { idRaw, name, teamName, teamType, nickname } = this;
        const id = idRaw;
        return { id, name, teamName, teamType, nickname };
    }
}
