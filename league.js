const { Coach } = require('./coaches.js');

/** A Blood Bowl league. */
class League {
    constructor(data) {
        this.name = data.name;
        this.ownerId = data.ownerId;
        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d) => new Coach(d));
        this.schedule = data.schedule.map((d) => new Round(d, this.coaches));

        // Ensure rounds are ordered by round number
        this.schedule.sort((l, r) => { l.id - r.id; });
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
     * @param {User} user - The user whose games will be found.
     * @return {Array<Game>} - All games that user plays in.
     */
    findUserGames(user) {
        return this.schedule.map((round) => round.findUserGame(user));
    }

    /**
     * @member {String} - The Discord mention string of the league owner.
     */
    get ownerMentionString() {
        return `<@${this.ownerId}>`;
    }
}

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
     * @param {User} user - The user whose games will be found.
     * @return {Array<Game>} - All games in this round that user plays in.
     */
    findUserGame(user) {
        return this.games.find((game) => game.coaches.some((coach) => coach.id === user.id));
    }
}

/** One match up in a Round. */
class Game {
    /**
     * @param {Object} data - Yaml representation of a game.
     */
    constructor(data, coaches) {
        this.home = data.home;
        this.away = data.away;
        this.homeCoach = coaches.find(c => c.teamName === data.home)
        this.awayCoach = coaches.find(c => c.teamName === data.away)
    }

    /** @member {Array<Coach>} - Both coaches playing in this game. */
    get coaches() {
        return [this.homeCoach, this.awayCoach];
    }

    getOpponent(user) {
        return this.coaches.find((c) => c.id !== user.id);
    }
}

module.exports = { League, Round, Game };
