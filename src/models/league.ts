import * as fs from 'fs';
import * as yaml from 'js-yaml';
import Discord from 'discord.js';

import { logger } from '@core/logger';
import { Option } from '@core/types/option';
import { Either } from '@core/types/either';
import { Result } from '@core/types/result';
import { PatternMatchable } from '@core/types/pattern';

import { Coach, CoachData } from './coach';
import { Round, RoundData } from './round';
import { Game } from './game';
import { processConfigValue } from './utils/config-reader';

const LOGGER = logger.child({module: 'league'});

export interface LeagueData {
    name: string;
    id: string;
    type: string;
    audienceId?: string | undefined;
    ownerId: string;
    currentRound: number;
    coaches: CoachData[];
    schedule: RoundData[];
}

class LeagueType extends PatternMatchable {
    constructor() {
        super([RoundRobin, Tournament]);
    }

    static RoundRobin() {
        return new RoundRobin();
    }

    static Tournament() {
        return new Tournament();
    }
}

class RoundRobin extends LeagueType {
    constructor() {
        super();
    }
}

class Tournament extends LeagueType {
    constructor() {
        super();
    }
}

class LeagueFactory {
    data: LeagueData;
    leagueFile: string;

    constructor(data: LeagueData, leagueFile: string) {
        this.data = data;
        this.leagueFile = leagueFile;
    }

    doit(): League {
        const leagueType = parseLeagueType(this.data.type).on({
            Ok: (type: LeagueType) => type,
            Err: (err) => { throw err; }
        });

        return leagueType.on({
            RoundRobin: () => new League(this.data, this.leagueFile),
            Tournament: () => new League(this.data, this.leagueFile),
        });
    }
}

/** A Blood Bowl league. */
export class League implements LeagueData {
    name: string;
    id: string;
    type: string;
    audienceId: string | undefined;
    ownerIdRaw: string;
    ownerId: string;
    currentRound: number;
    coaches: Coach[];
    schedule: Round[];
    leagueFile: string;

    constructor(data: LeagueData, leagueFile: string) {
        this.name = data.name;
        this.id = data.id;
        this.type = data.type;
        this.audienceId = data.audienceId;

        this.ownerIdRaw = data.ownerId;
        this.ownerId = processConfigValue(this.ownerIdRaw).on({
            Left: (v: string) => v,
            Right:(e: Error) => {throw e;}
        });

        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d) => new Coach(d));
        this.schedule = data.schedule.map((d) => new Round(d, this.coaches));
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
        return this.schedule
            .map((round) => round.findUserGame(user))
            .filter((game_or_none) => game_or_none.isSome())
            .map((game) => game.unwrap());
    }

    /**
     * Returns if user is registered in the league as a coach.
     *
     * @param {Discord.User} user
     * @return {bool}
     */
    protected userInLeague(user: Discord.User): boolean {
        return this.coaches.some((c) => c.id === user.id);
    }

    /**
     * Returns if user is registered in the league as a coach or as the owner.
     *
     * @param {Discord.User} user
     * @return {bool}
     */
    userInvolvedInLeague(user: Discord.User): boolean {
        return this.userInLeague(user) || this.ownerId === user.id;
    }

    /**
     * @member {String} - The Discord mention string of the league owner.
     */
    protected get ownerMentionString(): string {
        return `<@${this.ownerId}>`;
    }

    /**
     * @return {Option<Round>} - The next round or None if there are no more.
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

    encode(): LeagueData {
        const { id, name, type, ownerIdRaw, currentRound, coaches, schedule } = this;
        let {  audienceId } = this;
        const coachesData = coaches.map((c) => c.encode());
        const scheduleData = schedule.map((r) => r.encode());

        const ownerId = ownerIdRaw;

        return { id, name, type, ownerId, currentRound,
            coaches: coachesData,
            schedule: scheduleData,
            audienceId: audienceId || undefined
        };
    }
}

class RoundRobinSeason extends League {}
class TournamentSeason extends League {

    // DNC same
    // getCurrentRound(): Round {
    // getAudience(): Option<string> {

    /**
     * Finds all games that a user participates in this league.
     *
     * By nature of single elimination, this only searches the current round.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Array<Game>} - All games that user plays in.
     */
    findUserGames(user: Discord.User): Game[] {
        if (!this.userInLeague(user)) {
            return [];
        }

        return this.getCurrentRound().findUserGame(user).on({
            Some: (game) => [game],
            None: () => [],
        });
    }

    // DNC open question: should this exclude defeated players?
    protected userInLeague(user: Discord.User): boolean {
        return this.coaches.some((c) => c.id === user.id);
    }

    // DNC same
    // userInvolvedInLeague(user: Discord.User): boolean {

    // TODO this must return Either<Option<Round>, Err>
    // errors can occur when not all rounds reported winners to gen the next round
    incrementRound(): Option<Round> {
        // TODO games must declare winners too!
        return Option.None();
    }

    // DNC same
    // save(): void {
    // encode(): LeagueData {
}

export function getLeagueFromFile(leagueFile: string): League {
    const content = fs.readFileSync(leagueFile, 'utf-8');
    const data = yaml.load(content) as LeagueData;
    return LeagueFactory(data, leagueFile).doit();
}

function parseLeagueType(type_string: string): Result<LeagueType> {
    switch (type_string) {
        case "round-robin":
            return Result.Ok(LeagueType.RoundRobin());
        case "tournament":
            return Result.Ok(LeagueType.Tournament());
        default:
            return Result.Err(new Error(`Unknown league type ${type_string}`));
    }
}
