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
        this.done = data.done || false; // false if not-exists
        this.homeCoach = coaches.find(c => c.teamNameIsCloseEnough(data.home));
        this.awayCoach = coaches.find(c => c.teamNameIsCloseEnough(data.away));
    }

    /** @member {Array<Coach>} - Both coaches playing in this game. Order not guaranteed. Results may vary */
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

    encode() {
        const { home, away, done } = this;
        return { home, away, done };
    }
}

module.exports = { Game };
