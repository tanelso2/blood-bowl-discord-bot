const Discord = require('discord.js');

class StringFormat {

    static league(league) {
        const formatRound = (r) => this.round(r);
        const schedule = league.schedule.map(formatRound).join('\n\n');

        return `${league.name}\n\nCurrent round: ${league.currentRound}\n\n${schedule}`;
    }

    static round(round) {
        const title = `Round ${round.id}:`;

        const formatGame = (g) => this.game(g);
        const games = indent(round.games.map(formatGame).join('\n'));

        return `${title}\n${games}`;
    }

    static game(game) {
        const home = game.homeCoach;
        const away = game.awayCoach;

        const homeName = this.coach(home);
        const awayName = this.coach(away);

        return `${homeName} (${home.teamType}) v (${away.teamType}) ${awayName}`;
    }

    static coach(coach) {
        return coach.mentionString;
    }

    static coachAndTeam(coach) {
        return `${this.coach(coach).padEnd(12)} - ${coach.teamName.padEnd(16)} (${coach.teamType})`
    }
}


function indent(s) {
    const pad = (line) => (line !== '') ? '  '.concat(line) : line;
    return s.split('\n').map(pad).join('\n');
}


/** Formats league structures for Discord. */
const BLANK = '\u200b';
class DiscordFormat {


    /**
     * Creates the message for advancing a round.
     *
     * @param {Round} newRound
     * @return {Discord.MessageEmbed}
     */
    static roundAdvance(newRound) {
        const result = this.MessageEmbed()
            .setTitle(`Round ${newRound.id} Matchups`);
        newRound.games.forEach((g) => result.addField(this.makeMatchupField(g)));
        return result;
    }

    /**
     * Creates the field object for a game.
     *
     * @method
     * @private
     * @param {Game} - The game to create an embed field for.
     * @return {Object} - The embed field to add for this game.
     */
    static makeMatchupField(game) {
        return { name: BLANK, value: StringFormat.game(game) };
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
                { name: 'Home', value: game.homeCoach.teamName, inline: true },
                { name: 'Away', value: game.awayCoach.teamName, inline: true },
            )
            .addFields(
                { name: 'Coach', value: StringFormat.coach(game.homeCoach), inline: true },
                { name: 'Coach', value: StringFormat.coach(game.awayCoach), inline: true },
            );
    }

    /**
     * Formats a coach.
     *
     * @param {Coache} coach
     * @return {String}
     */
    static coach(coach) {
        return StringFormat.coach(coach);
    }

    /**
     * Creates the response for a user's upcoming games.
     *
     * @param {Discord.User} user
     * @param {Array<Game>} usersGames - Matches in this league for the user in order.
     * @return {String}
     */
    static usersSchedule(user, usersGames, currentRoundNumber) {
        const formatLeader = function (roundId) {
            const caret = roundId === currentRoundNumber ? '>' : ' ';
            return `${caret}${roundId.toString().padStart(2)}.`;
        }
        const formatOpponent = function (game) {
            const opponent = game.getOpponent(user);
            return StringFormat.coachAndTeam(opponent);
        }

        const formatMatch = (roundId, game) => `${formatLeader(roundId)} ${formatOpponent(game)}`;
        return Array.from(usersGames.entries(), ([idx, game]) => formatMatch(idx + 1, game)).join('\n');
    }

    /**
     * Creates a new embed with the bot's defaults applied.
     *
     * @return {Discord.MessageEmbed}
     */
    static MessageEmbed() {
        return new Discord.MessageEmbed({
                type: 'rich',
                color: 'RED',
            });
    }

    /**
     * Wraps content in a code block.
     */
    static makeCodeBlock(s) {
        const border = '```';
        return `${border}\n${s}${border}`;
    }
}

module.exports = { StringFormat, DiscordFormat };
