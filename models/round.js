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
     * @return {Array<Game>} - All games in this round that user plays in.
     */
    findUserGame(user) {
        return this.games.find((game) => game.coaches.some((coach) => coach.id === user.id));
    }
}

module.exports = { Round };