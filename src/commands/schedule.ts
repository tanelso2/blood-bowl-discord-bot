import { generateInsult } from '@generator/helpers';
import { assertLeague, CommandContext } from './core';

/*
 * Example output:
 *  @user, here is your schedule this league:
 *  ```
 *  1. ${opponent.commonName}     - ${opponent.teamName}     (${opponent.teamType})
 * >2. ${nextOpponent.commonName} - ${nextOpponent.teamName} (${nextOpponent.teamType})
 *  3. etc....
 *  ```
 */
export async function printSchedule(context: CommandContext) {
    const { league, user, message, formatter } = context;
    assertLeague(league);

    const matches = league.findUserGames(user);
    if (matches.length) {
        const schedule = formatter.usersSchedule(user, matches, league.currentRound);
        const content = `here is your schedule this league:\n${schedule}`;
        return message.reply({
            content, 
            allowedMentions: {
                // Don't mention anyone else when printing a user's schedule
                parse: []
            }
        });
    }
    const insult = generateInsult();
    return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
}