const { Game } = require('./game.js')

/** One round of a League. */
class Round {
    /**
     * @param {Object} data - Yaml representation of a round.
     * @param {Array<Coach>} coaches
     */
    constructor(data, coaches) {
        this.id = data.round;
        this.games = data.games.map((d) => new Game(d, coaches));
    }

    /**
     * Finds all games that a user participates in this round.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Game} - The game in this round that user plays in. 
     * If the user is in multiple games in a round, this method has undefined behavior
     * 
     */
    findUserGame(user) {
        return this.games.find((game) => game.coaches.some((coach) => coach.id === user.id));
    }

    getUnfinishedGames() {
        return this.games.filter((game) => !game.done);
    }

    encode() {
        const round = this.id;
        const games = this.games.map((g) => g.encode());
        return { round, games };
    }
}

module.exports = { Round };