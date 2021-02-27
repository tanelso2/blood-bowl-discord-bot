import * as fs from 'fs';
import * as yaml from 'js-yaml';
import Discord from 'discord.js';

import { logger } from '@core/logger';
import { Option } from '@core/types/option';

import { Coach } from './coach';
import { Round } from './round';
import { Game } from './game';
import { processConfigValue } from './utils/config-reader';

const LOGGER = logger.child({module: 'league'});

/** A Blood Bowl league. */
export class League {
    name: string;
    id: string;
    audienceId: string;
    ownerIdRaw: string;
    ownerId: string;
    currentRound: number;
    coaches: Coach[];
    schedule: Round[];
    leagueFile: string;

    constructor(data: any, leagueFile: string) {
        this.name = data.name;
        this.id = data.id;
        this.audienceId = data.audienceId;

        //OwnerId
        this.ownerIdRaw = data.ownerId || data.ownerID;
        this.ownerId = processConfigValue(this.ownerIdRaw).on({
            Left: (v: string) => v,
            Right:(e: Error) => {throw e;}
        });

        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d: any) => new Coach(d));
        this.schedule = data.schedule.map((d: any) => new Round(d, this.coaches));
        this.leagueFile = leagueFile;

        // Ensure rounds are ordered by round number
        this.schedule.sort((l: Round, r: Round) => l.id - r.id);
    }

    /**
     * @return {Round} - The active round.
     */
    getCurrentRound(): Round {
        return this.schedule[this.currentRound - 1];
    }

    /**
     * @return {Option<String>} - The name of the group to ping, or Nothing if none is specified
     */
    getAudience(): Option<string> {
        const audienceId = this.audienceId;

        if (!audienceId) {
            return Option.None();
        }

        return Option.Some(audienceId);
    }

    matches(specifier: string): boolean {
        return specifier === this.id;
    }

    /**
     * Finds all games that a user participates in this league.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Array<Game>} - All games that user plays in.
     */
    findUserGames(user: Discord.User): Game[] {
        logger.debug(`fetching games for ${user.username}(${user.id})`);
        if (!this.userInLeague(user)) {
            return [];
        }
        return this.schedule.map((round) => round.findUserGame(user));
    }

    /**
     * Returns if user is registered in the league as a coach.
     *
     * @param {Discord.User} user
     * @return {bool}
     */
    userInLeague(user: Discord.User): boolean {
        return this.coaches.some((c) => c.id === user.id);
    }

    userInvolvedInLeague(user: Discord.User): boolean {
        return this.userInLeague(user) || this.ownerId === user.id;
    }

    /**
     * @member {String} - The Discord mention string of the league owner.
     */
    get ownerMentionString(): string {
        return `<@${this.ownerId}>`;
    }

    /**
     * @return {Option<Round>}
     */
    incrementRound(): Option<Round> {
        const numRounds = this.schedule.length;
        const newRound = this.currentRound + 1;
        if (newRound > numRounds) {
            return Option.None();
        }
        this.currentRound = newRound;
        this.save();
        return Option.Some(this.getCurrentRound());
    }

    save(): void {
        console.log(this.encode());
        const yamlStr = yaml.dump(this.encode());
        fs.writeFileSync(this.leagueFile, yamlStr, 'utf8');
    }

    encode(): any {
        const { id, name, ownerIdRaw, currentRound } = this;
        let { coaches, schedule, audienceId } = this;
        coaches = coaches.map((c) => c.encode());
        schedule = schedule.map((r) => r.encode());

        const ownerId = ownerIdRaw;

        return { id, name, ownerId, currentRound, coaches, schedule,
            audienceId: audienceId || null
        };
    }
}

export function getLeagueFromFile(leagueFile: string): League {
    const content = fs.readFileSync(leagueFile, 'utf-8');
    const data = yaml.load(content);
    return new League(data, leagueFile);
}
