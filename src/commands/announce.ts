import { Game } from '@models/game';
import { assertLeague, CommandContext } from './core';

export async function announceGame(context: CommandContext) {
    const { league, user, message } = context;
    assertLeague(league);

    const usersGame = league.getCurrentRound().findUserGame(user.id);
    return usersGame.on({
        Some: (game: Game) => {
            const audienceString = league.getAudience().on({
                Some: (roleId: string) => `<@&${roleId}>`,
                None: () => '@here'
            });
            const [homeCoach, awayCoach] = game.coaches;

            return message.channel.send(`${audienceString} - ${homeCoach.teamType} v. ${awayCoach.teamType}`);
        },
        None: () => message.reply("you don't seem to be playing this round, smoothbrain."),
    });
}