const { Coach } = require('./coaches.js');
const { Schedule } = require('./schedule.js');

class League {
    constructor(data) {
        this.name = data.name;
        this.ownerId = data.ownerId;
        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map(makeCoach);
        this.schedule = new Schedule(data.schedule);
    }

    getCurrentRound() {
        return this.schedule.rounds[this.currentRound - 1];
    }

    get ownerMentionString() {
        return `<@${this.ownerId}>`;
    }
}

// Mappable constructor
function makeCoach(data) {
    return new Coach(data);
}

module.exports = { League };
