import { Game } from '@models/game';
import * as insultGenerator from '@generator/string-generator';
import { assertLeague, CommandContext } from './core';

export async function markGameDone(context: CommandContext) {
    const { league, message, user } = context;
    assertLeague(league);

    function markDone(finishedGame: Game) {
        finishedGame.done = true;
        league!.save();
    }

    function respond(finishedGame: Game) {
        const numUnfinishedGames = currentRound.getUnfinishedGames().length;
        const opponent = finishedGame.getOpponent(user);
        const isOrAre = numUnfinishedGames === 1 ? "is" : "are";
        return message.reply(`Gotcha. Your game against ${opponent.commonName} has been recorded. ` +
                             `There ${isOrAre} ${numUnfinishedGames} left in the round.`);
    }

    function insult() {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`You don't appear to be playing this round...\n${insult}`);
    }

    const currentRound = league.getCurrentRound();
    return currentRound.findUserGame(user.id).on({
        Some: (game: Game) => {
            markDone(game);
            return respond(game);
        },
        None: () => insult(),
    });
}