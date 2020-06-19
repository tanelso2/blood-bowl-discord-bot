const logger = require('../logger.js').child({ module: 'league' });
const { Coach } = require('./coaches.js');
const { Round } = require("./round.js");

/** A Blood Bowl league. */
class League {
    constructor(data) {
        this.name = data.name;
        this.ownerId = data.ownerId;
        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d) => new Coach(d));
        this.schedule = data.schedule.map((d) => new Round(d, this.coaches));

        // Ensure rounds are ordered by round number
        this.schedule.sort((l, r) => l.id - r.id);
    }

    /**
     * @return {Round} - The active round.
     */
    getCurrentRound() {
        return this.schedule[this.currentRound - 1];
    }

    /**
     * Finds all games that a user participates in this league.
     *
     * @param {Discord.User} user - The user whose games will be found.
     * @return {Array<Game>} - All games that user plays in.
     */
    findUserGames(user) {
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
    userInLeague(user) {
        return this.coaches.some((c) => c.id === user.id);
    }

    /**
     * @member {String} - The Discord mention string of the league owner.
     */
    get ownerMentionString() {
        return `<@${this.ownerId}>`;
    }
}

module.exports = { League };
