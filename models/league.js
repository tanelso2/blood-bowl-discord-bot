const fs = require('fs');
const yaml = require('js-yaml');
const logger = require('../logger.js').child({ module: 'league' });
const { Coach } = require('./coaches.js');
const { Round } = require("./round.js");
const { processConfigValue } = require("./utils/config-reader.js");
const { Option } = require("../utils/types/option.js");

/** A Blood Bowl league. */
class League {
    constructor(data, leagueFile) {
        this.name = data.name;
        this.id = data.id;
        this.audienceId = data.audienceId;

        //OwnerId
        this.ownerIdRaw = data.ownerId || data.ownerID;
        this.ownerId = processConfigValue(this.ownerIdRaw).on({
            Left: (v) => v,
            Right:(e) => {throw e;}
        });

        this.currentRound = data.currentRound;
        this.coaches = data.coaches.map((d) => new Coach(d));
        this.schedule = data.schedule.map((d) => new Round(d, this.coaches));
        this.leagueFile = leagueFile;

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
     * @return {Option<String>} - The name of the group to ping, or Nothing if none is specified
     */
    getAudience() {
        const audienceId = this.audienceId;

        if (!audienceId) {
            return Option.None();
        }

        return Option.Some(audienceId);
    }

    matches(specifier) {
        return specifier === this.id;
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

    userInvolvedInLeague(user) {
        return this.userInLeague(user) || this.ownerId === user.id;
    }

    /**
     * @member {String} - The Discord mention string of the league owner.
     */
    get ownerMentionString() {
        return `<@${this.ownerId}>`;
    }

    /**
     * @return {Option<Round>}
     */
    incrementRound() {
        const numRounds = this.schedule.length;
        const newRound = this.currentRound + 1;
        if (newRound > numRounds) {
            return Option.None();
        }
        this.currentRound = newRound;
        this.save();
        return Option.Some(this.getCurrentRound());
    }

    save() {
        console.log(this.encode());
        const yamlStr = yaml.safeDump(this.encode());
        fs.writeFileSync(this.leagueFile, yamlStr, 'utf8');
    }

    encode() {
        const { id, name, ownerIdRaw, currentRound } = this;
        let { coaches, schedule, audienceId } = this;
        coaches = coaches.map((c) => c.encode());
        schedule = schedule.map((r) => r.encode());

        if (!audienceId) {
            audienceId = null
        }

        const ownerId = ownerIdRaw;

        return { audienceId, id, name, ownerId, currentRound, coaches, schedule };
    }
}

function getLeagueFromFile(leagueFile) {
    const content = fs.readFileSync(leagueFile);
    const data = yaml.safeLoad(content);
    return new League(data, leagueFile);
}

module.exports = { League, getLeagueFromFile };
