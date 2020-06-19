const logger = require('../logger.js').child({ module: 'game' });
const { Coach } = require('./coaches.js');

/** One match up in a Round. */
class Game {
    /**
     * @param {Object} data - Yaml representation of a game.
     */
    constructor(data, coaches) {
        this.home = data.home;
        this.away = data.away;
        this.homeCoach = coaches.find(c => c.teamName === data.home);
        this.awayCoach = coaches.find(c => c.teamName === data.away);
    }

    /** @member {Array<Coach>} - Both coaches playing in this game. */
    get coaches() {
        return [this.homeCoach, this.awayCoach];
    }

    /**
     * Gets the opposing coach for this game.
     *
     * @param {Discord.User} user
     * @return {Coach}
     */
    getOpponent(user) {
        const opponentOrUndef = this.coaches.find((c) => c.id !== user.id);
        if (opponentOrUndef) {
            return opponentOrUndef;
        }
        logger.error(`unknown opponent of ${user.id}`);
        return Coach.null();
    }
}

module.exports = { Game };