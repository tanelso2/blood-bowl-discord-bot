import * as insultGenerator from '@generator/string-generator';
import { Game } from "@models/game";
import { assertLeague, CommandContext } from "./core";

/*
 * Example output:
 *  @user you are playing @opponent this round
 */
export async function findOpponent(context: CommandContext) {
    const { user, league, message, formatter } = context;
    assertLeague(league);

    const userInGame = (game: Game) => game.coaches.some((c) => c.id === user.id);
    const usersGame = league.getCurrentRound().games.find(userInGame);

    if (usersGame === undefined) {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
    }

    const opponent = usersGame.getOpponent(user);
    return message.reply(`you are playing ${formatter.coach(opponent)} this round.`);
}