const { Coach } = require('./coaches.js');

class League {
    constructor(data) {
        this.name = data.name;
        this.ownerId = data.ownerId;
        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d) => new Coach(d));
        this.schedule = data.schedule.map((d) => new Round(d, this.coaches));
        this.schedule.sort((l, r) => { l.id - r.id; });
    }

    getCurrentRound() {
        return this.schedule[this.currentRound - 1];
    }

    get ownerMentionString() {
        return `<@${this.ownerId}>`;
    }
}

class Round {
    constructor(data, coaches) {
        this.id = data.round;
        this.games = data.games.map((d) => new Game(d, coaches));
    }
}

class Game {
    constructor(data, coaches) {
        this.home = data.home;
        this.away = data.away;
        this.homeCoach = coaches.find(c => c.teamName === data.home)
        this.awayCoach = coaches.find(c => c.teamName === data.away)
    }

    get coaches() {
        return [this.homeCoach, this.awayCoach];
    }
}

module.exports = { League, Round, Game };
