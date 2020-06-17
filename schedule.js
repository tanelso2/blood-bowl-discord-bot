class Schedule {
    constructor(data) {
        this.rounds = data.map(makeRound);
        this.rounds.sort((l, r) => { l.id - r.id; });
    }
}

class Round {
    constructor(data) {
        this.id = data.round;
        this.games = data.games.map(makeGame);
    }
}

class Game {
    constructor(data) {
        this.home = data.home;
        this.away = data.away;
    }

    get coaches() {
        return [this.home_coach, this.away_coach];
    }
}

// Mappable constructors
function makeRound(data) {
    return new Round(data);
}

function makeGame(data) {
    return new Game(data);
}

module.exports = { Schedule };
