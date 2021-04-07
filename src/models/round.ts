import { Game, GameData } from './game';
import { Coach } from './coach';
import { Option } from "@core/types/option";
import Discord from 'discord.js';

export interface RoundData {
    round: number;
    games: GameData[];
}

/** One round of a League. */
export class Round implements RoundData {
    id: number;
    round: number;
    games: Game[];

    /**
     * @param {Object} data - Yaml representation of a round.
     * @param {Array<Coach>} coaches
     */
    constructor(data: RoundData, coaches: Coach[]) {
        this.id = data.round;
        this.round = data.round;
        this.games = data.games.map((d) => new Game(d, coaches));
    }

    /**
     * Finds all games that a user participates in this round.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Option<Game>} - The game in this round that user plays in, or None.
     * If the user is in multiple games in a round, this method has undefined behavior
     *
     */
    findUserGame(user: Discord.User): Option<Game> {
        return Option.ofNullable(this.games.find((game) => game.coaches.some((coach) => coach.id === user.id)));
    }

    getUnfinishedGames(): Game[] {
        return this.games.filter((game) => !game.done);
    }

    encode(): RoundData {
        const round = this.id;
        const games = this.games.map((g) => g.encode());
        return { round, games };
    }
}
