import { Game, GameData } from './game';
import { Coach } from './coach';
import { Option } from "@core/types/generated/option";

export interface RoundData {
    round: number;
    games: GameData[];
    startTime?: number | undefined;
}

/** One round of a League. */
export class Round implements RoundData {
    id: number;
    round: number;
    games: Game[];
    startTime: number | undefined;

    /**
     * @param {Object} data - Yaml representation of a round.
     * @param {Array<Coach>} coaches
     */
    constructor(data: RoundData, coaches: Coach[]) {
        this.id = data.round;
        this.round = data.round;
        this.games = data.games.map((d) => new Game(d, coaches));
        this.startTime = data.startTime;
    }

    /**
     * Finds all games that a user participates in this round.
     *
     * @param {string} userId - The user whose games will be found.
     * @return {Option<Game>} - The game in this round that user plays in, or None.
     * If the user is in multiple games in a round, this method has undefined behavior
     *
     */
    findUserGame(userId: string): Option<Game> {
        return Option.ofNullable(this.games.find((game) => game.coaches.some((coach) => coach.id === userId)));
    }

    getUnfinishedGames(): Game[] {
        return this.games.filter((game) => !game.done);
    }

    getRoundStart(): Option<number> {
        return Option.ofNullable(this.startTime)
    }

    setRoundStart() {
        this.startTime = Date.now();
    }

    encode(): RoundData {
        const round = this.id;
        const games = this.games.map((g) => g.encode());
        const startTime = this.startTime || undefined;
        return { round, games, startTime };
    }
}
