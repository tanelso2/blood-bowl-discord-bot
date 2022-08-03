import * as insultGenerator from '@generator/string-generator';
import { Game } from '@models/game';
import { assertLeague, CommandContext } from './core';

export function declareWinner(context: CommandContext) {
    const { league, user, message } = context;
    assertLeague(league);

    const currentRound = league.getCurrentRound();
    return currentRound.findUserGame(user.id).match({
        Some: (finishedGame: Game) => {
            finishedGame.declareWinner(user.id);
            league.save();
            const opponent = finishedGame.getOpponent(user);
            const slight = insultGenerator.generateString("${slight}");
            return message.reply(`Nice, recorded your win against ${opponent.commonName}, that ${slight}.`);
        },
        None: () => message.reply(insultGenerator.generateString("You don't appear to be playing this round...${slight}")),
    });
}