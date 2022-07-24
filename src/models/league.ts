import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as yaml from 'js-yaml';
import Discord from 'discord.js';

import { logger } from '@core/logger';
import { Option } from '@core/types/option';
import { Either } from '@core/types/either';
import { Result } from '@core/types/result';

import { Coach, CoachData } from './coach';
import { Round, RoundData } from './round';
import { Game, GameData } from './game';
import { processConfigValue } from './utils/config-reader';

//const LOGGER = logger.child({module: 'league'});

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

enum LeagueType {
    RoundRobin,
    Tournament
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

        switch (leagueType) {
            case LeagueType.RoundRobin:
                return new RoundRobinSeason(this.data, this.leagueFile);
            case LeagueType.Tournament:
                return new TournamentSeason(this.data, this.leagueFile);
            default:
                throw new Error(`Unhandled and unknown leagueType ${leagueType as string}`);
        }
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
     * @return {Option<Round>} - The active round, if it exists.
     */
    getCurrentRound(): Option<Round> {
        return Option.ofNullable(this.schedule[this.currentRound - 1]);
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
            .map((round) => round.findUserGame(user.id))
            .filter((game_or_none) => game_or_none.isSome())
            .map((game) => game.unwrap());
    }

    findUserCurrentGame(userId: string): Option<Game> {
        return this.getCurrentRound().flatMap(x => x.findUserGame(userId));
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
     * @return {Either<Round>} - The next round or an error if there are no more.
     */
    incrementRound(): Either<Error, Round> {
        const numRounds = this.schedule.length;
        const newRound = this.currentRound + 1;
        if (newRound > numRounds) {
            return Either.Left(new Error("Could not advance, that was the last round"));
        }
        this.currentRound = newRound;
        this.save();
        return this.getCurrentRound().on({
            None: () => Either.Left(new Error("Advanced the round, but the new round is undefined")),
            Some: (r: Round) => Either.Right(r)
        });
    }

    save(): void {
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
export class TournamentSeason extends League {

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

        return this.getCurrentRound().map(x => x.findUserGame(user.id)).on({
            Some: (game: Game) => [game],
            None: () => [],
        });
    }

    protected userInLeague(user: Discord.User): boolean {
        return this.coaches.some((c) => c.id === user.id);
    }

    // errors can occur when not all rounds reported winners to gen the next round
    incrementRound(): Either<Error, Round> {
        return this.getCurrentRound().on({
            Some: (currentRound: Round) => {
                if(!currentRound.games.every((x) => x.hasWinner())) {
                    return Either.Left(new Error("Cannot advance current round, every match needs a winner declared"));
                }
                const winners = currentRound.games.map((x) => x.winner!);
                if (winners.length === 1) {
                    return Either.Left(new Error("Cannot advance round, the champion has been declaredddddd"));
                }
                const matchups: GameData[] = [];
                for (let i = 0; i < winners.length; i+=2) {
                    const home = winners[i];
                    const away = winners[i+1];
                    matchups.push({home, away});
                }
                const roundData: RoundData = {
                    round: currentRound.round + 1,
                    games: matchups
                };
                const r = new Round(roundData, this.coaches);
                this.schedule.push(r);
                this.currentRound += 1;

                return Either.Right(this.getCurrentRound());
                },
                None: () => {
                    this.currentRound = 1;
                    return Either.Right(this.getCurrentRound().unwrap());
                }
        });
    }

    static makeFirstRoundGivenRankings(rankings: CoachData[]): RoundData {
        // I swear I tried to figure out the algorithm for this, but couldn't get it
        // Hard code instead
        switch (rankings.length) {
            case 4:
                return {
                    round: 1,
                    games: [
                        {home: rankings[0].teamName, away: rankings[3].teamName},
                        {home: rankings[1].teamName, away: rankings[2].teamName}
                    ]
                };
            case 8:
                return {
                    round: 1,
                    games: [
                        {home: rankings[0].teamName, away: rankings[7].teamName},
                        {home: rankings[3].teamName, away: rankings[4].teamName},
                        {home: rankings[2].teamName, away: rankings[5].teamName},
                        {home: rankings[1].teamName, away: rankings[6].teamName}
                    ]
                };
            default:
                throw new Error(`Cannot have a tournament with ${rankings.length} coaches`);
        }
    }
}

export function getLeagueFromFile(leagueFile: string): League {
    const content = fs.readFileSync(leagueFile, 'utf-8');
    const data = yaml.load(content) as LeagueData;
    return new LeagueFactory(data, leagueFile).doit();
}

export function makeTmpFileLeague(data: LeagueData): League {
    const randomName = `tmp-league-${Math.floor(Math.random() * Math.floor(10000))}`;
    const tmpFile = path.join(os.tmpdir(), randomName);
    return new LeagueFactory(data, tmpFile).doit();
}

function parseLeagueType(type_string: string): Result<LeagueType> {
    switch (type_string) {
        case "round-robin":
            return Result.Ok(LeagueType.RoundRobin);
        case "tournament":
            return Result.Ok(LeagueType.Tournament);
        default:
            return Result.Err(new Error(`Unknown league type ${type_string}`));
    }
}
