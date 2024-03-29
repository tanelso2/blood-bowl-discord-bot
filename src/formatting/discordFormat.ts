import Discord, { EmbedBuilder } from 'discord.js';
import { Game } from '@models/game';
import { Round } from '@models/round';
import { Coach } from '@models/coach';
import { timeStringDaysElapsed, TimeUnit } from '@utils/timeUtils';

const BLANK = '\u200b';

export const MAX_LENGTH = ''

interface MatchupField {
    name: string;
    value: string;
}

/** Formats league structures for Discord. */
export class DiscordFormat {
    private client: Discord.Client;

    /**
     * @param {Discord.Client} client
     */
    constructor(client: Discord.Client) {
        this.client = client;
    }

    /**
     * Creates the message for advancing a round.
     *
     * @param {Round} newRound
     * @return {Discord.EmbedBuilder}
     */
    roundAdvance(newRound: Round): Discord.EmbedBuilder {
        return this.defaultEmbed()
            .setTitle(`Round ${newRound.id} Matchups`)
            .addFields(newRound.games.map((x) => this.makeMatchupField(x)));
    }

    /**
     * Creates the message for the current status of a round.
     * 
     * @param {Round} round
     * @return {Discord.EmbedBuilder}
     */
    roundStatus(round: Round): Discord.EmbedBuilder {
        const gameFields = round.games.map((g) => {
            const { homeCoach, awayCoach } = g;
            const ret = `${homeCoach.commonName} (${homeCoach.teamType}) v (${awayCoach.teamType}) ${awayCoach.commonName}`;
            const value = g.done ? `~~${ret}~~` : ret;
            return { name: BLANK, value };
        });
        let ret = this.defaultEmbed()
            .setTitle(`Round ${round.id}`)
            .addFields(gameFields);
        round.getRoundStart().match({
            Some: (startTime) => {
                const timeElapsed = Date.now() - startTime;
                const value = `It's been ${timeStringDaysElapsed(timeElapsed, TimeUnit.Milliseconds)} since the round started.`;
                ret = ret.addFields([{ name: BLANK, value }]);
            },
            None: () => {}
        });
        return ret;
    }


    /**
     * Creates the field object for a game.
     *
     * @method
     * @private
     * @param {Game} - The game to create an embed field for.
     * @return {Object} - The embed field to add for this game.
     */
    private makeMatchupField(game: Game): MatchupField {
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
     * @return {Discord.EmbedBuilder}
     */
    game(game: Game): Discord.EmbedBuilder {
        const getCoach = (x: Coach) => this.coach(x);
        return this.defaultEmbed()
            .setTitle(`${game.homeCoach.teamName} v ${game.awayCoach.teamName}`)
            .addFields(
                {name: 'Home', value: game.homeCoach.teamName, inline: true},
                {name: 'Away', value: game.awayCoach.teamName, inline: true},
            )
            .addFields(
                {name: 'Coach', value: getCoach(game.homeCoach), inline: true},
                {name: 'Coach', value: getCoach(game.awayCoach), inline: true},
            );
    }

    /**
     * Formats a coach.
     *
     * @param {Coach} coach
     * @return {String}
     */
    coach(coach: Coach): string {
        return coach.mentionString;
    }

    /**
     * Formats a coach with their team name and type.
     *
     * @param {Coach} coach
     * @return {String}
     */
    coachAndTeam(coach: Coach): string {
        return `${coach.commonName.padEnd(12)} - ${coach.teamName.padEnd(16)} (${coach.teamType})`
    }

    /**
     * Creates the response for a user's upcoming games.
     */
    usersSchedule(user: Discord.User, usersGames: Game[], currentRoundNumber: number): string {
        const formatLeader = function (roundId: number) {
            const caret = roundId === currentRoundNumber ? '>' : ' ';
            return `${caret}${roundId.toString().padStart(2)}.`;
        }

        // OK I still hate JS, 'this' binding is broken so just capture it here
        const formatCoachAndTeam = (x: Coach) => this.coachAndTeam(x);

        const formatOpponent = function (game: Game) {
            const opponent = game.getOpponent(user);
            return formatCoachAndTeam(opponent);
        }

        const formatMatch = (roundId: number, game: Game) => `${formatLeader(roundId)} ${formatOpponent(game)}`;

        const result = Array.from(usersGames.entries(), ([idx, game]) => formatMatch(idx + 1, game)).join('\n');
        return this.makeCodeBlock(result);
    }

    /**
     * Creates a new embed with the bot's defaults applied.
     *
     * @return {Discord.EmbedBuilder}
     */
    defaultEmbed(): EmbedBuilder {
        const currentUser = this.client.user as Discord.User;
        return new Discord.EmbedBuilder()
            .setColor('Red')
            .setAuthor({name: currentUser.username, iconURL: currentUser.displayAvatarURL()})
            .setTimestamp();

    }

    /**
     * Wraps content in a code block.
     */
    makeCodeBlock(s: string): string {
        const border = '```';
        return `${border}\n${s}${border}`;
    }
}
