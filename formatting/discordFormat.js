const Discord = require('discord.js');

const BLANK = '\u200b';

/** Formats league structures for Discord. */
class DiscordFormat {

    /**
     * @param {Discord.Client} client
     */
    constructor(client) {
        this.client = client;
    }

    /**
     * Creates the message for advancing a round.
     *
     * @param {Round} newRound
     * @return {Discord.MessageEmbed}
     */
    roundAdvance(newRound) {
        return this.MessageEmbed()
            .setTitle(`Round ${newRound.id} Matchups`)
            .addFields(newRound.games.map(this.makeMatchupField, this));
    }

    /**
     * Creates the message for the current status of a round.
     * 
     * @param {Round} round
     * @return {Discord.MessageEmbed}
     */
    roundStatus(round) {
        const gameFields = round.games.map((g) => {
            const { homeCoach, awayCoach } = g;
            const ret = `${homeCoach.commonName} (${homeCoach.teamType}) v (${awayCoach.teamType}) ${awayCoach.commonName}`;
            const value = g.done ? `~~${ret}~~` : ret;
            return { name: BLANK, value };
        });
        return this.MessageEmbed()
            .setTitle(`Round ${round.id}`)
            .addFields(gameFields);
    }

    /**
     * Creates the field object for a game.
     *
     * @method
     * @private
     * @param {Game} - The game to create an embed field for.
     * @return {Object} - The embed field to add for this game.
     */
    makeMatchupField(game) {
        const home = game.homeCoach;
        const away = game.awayCoach;

        const homeName = this.coach(home);
        const awayName = this.coach(away);

        return {
            name: BLANK,
            value: `${homeName} (${home.teamType}) v (${away.teamType}) ${awayName}`,
        };
    }

    /**
     * Creates the message for describing a game.
     *
     * @param {Game} game
     * @return {Discord.MessageEmbed}
     */
    static game(game) {
        return this.MessageEmbed()
            .setTitle(`${game.homeCoach.teamName} v ${game.awayCoach.teamName}`)
            .addFields(
                {name: 'Home', value: game.homeCoach.teamName, inline: true},
                {name: 'Away', value: game.awayCoach.teamName, inline: true},
            )
            .addFields(
                {name: 'Coach', value: this.coach(game.homeCoach), inline: true},
                {name: 'Coach', value: this.coach(game.awayCoach), inline: true},
            );
    }

    /**
     * Formats a coach.
     *
     * @param {Coach} coach
     * @return {String}
     */
    coach(coach) {
        return coach.mentionString;
    }

    /**
     * Formats a coach with their team name and type.
     *
     * @param {Coach} coach
     * @return {String}
     */
    coachAndTeam(coach) {
        return `${coach.commonName.padEnd(12)} - ${coach.teamName.padEnd(16)} (${coach.teamType})`
    }

    /**
     * Creates the response for a user's upcoming games.
     *
     * @param {Discord.User} user
     * @param {Array<Game>} usersGames - Matches in this league for the user in order.
     * @return {String}
     */
    usersSchedule(user, usersGames, currentRoundNumber) {
        const formatLeader = function (roundId) {
            const caret = roundId === currentRoundNumber ? '>' : ' ';
            return `${caret}${roundId.toString().padStart(2)}.`;
        }

        // OK I still hate JS, 'this' binding is broken so just capture it here
        const formatCoachAndTeam = this.coachAndTeam;

        const formatOpponent = function (game) {
            const opponent = game.getOpponent(user);
            return formatCoachAndTeam(opponent);
        }

        const formatMatch = (roundId, game) => `${formatLeader(roundId)} ${formatOpponent(game)}`;

        const result = Array.from(usersGames.entries(), ([idx, game]) => formatMatch(idx + 1, game)).join('\n');
        return this.makeCodeBlock(result);
    }

    /**
     * Creates a new embed with the bot's defaults applied.
     *
     * @return {Discord.MessageEmbed}
     */
    MessageEmbed() {
        return new Discord.MessageEmbed()
            .setColor('RED')
            .setAuthor(this.client.user.username, this.client.user.displayAvatarURL())
            .setTimestamp();
    }

    /**
     * Wraps content in a code block.
     */
    makeCodeBlock(s) {
        const border = '```';
        return `${border}\n${s}${border}`;
    }
}

module.exports = { DiscordFormat };
