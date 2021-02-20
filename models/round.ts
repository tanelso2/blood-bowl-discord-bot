import { Game } from './game';
import { Coach } from './coach';
import Discord from 'discord.js';

/** One round of a League. */
export class Round {
    id: number;
    games: Game[];

    /**
     * @param {Object} data - Yaml representation of a round.
     * @param {Array<Coach>} coaches
     */
    constructor(data: any, coaches: Coach[]) {
        this.id = data.round;
        this.games = data.games.map((d: any) => new Game(d, coaches));
    }

    /**
     * Finds all games that a user participates in this round.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Game} - The game in this round that user plays in. 
     * If the user is in multiple games in a round, this method has undefined behavior
     * 
     */
    findUserGame(user: Discord.User): Game {
        return this.games.find((game) => game.coaches.some((coach) => coach.id === user.id)) as Game;
    }

    getUnfinishedGames(): Game[] {
        return this.games.filter((game) => !game.done);
    }

    encode(): any {
        const round = this.id;
        const games = this.games.map((g) => g.encode());
        return { round, games };
    }
}
