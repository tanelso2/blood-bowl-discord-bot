class Schedule {
    constructor(data, coaches) {
        function makeRound(data) {
            return new Round(data, coaches);
        }
        this.rounds = data.map(makeRound);
        this.rounds.sort((l, r) => { l.id - r.id; });
    }
}

class Round {
    constructor(data, coaches) {
        function makeGame(data) {
            return new Game(data, coaches);
        }
        this.id = data.round;
        this.games = data.games.map(makeGame);
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

// Mappable constructors


module.exports = { Schedule, Round, Game };
