import { generateSlight } from '@generator/helpers';
import { Game } from '@models/game';
import { assertLeague, CommandContext } from './core';

function extractUserId(s: string): string {
    return s.slice(2, -1);
}

export function declareWinner(context: CommandContext) {
    const { league, user, message, restOfMessage } = context;
    assertLeague(league);

    let userId = user.id;
    if (restOfMessage.length > 0) {
        userId = extractUserId(restOfMessage[0]);
    }

    const currentRound = league.getCurrentRound();
    return currentRound.findUserGame(userId).match({
        Some: (finishedGame: Game) => {
            finishedGame.declareWinner(userId);
            league.save();
            const opponent = finishedGame.getOpponentFromId(userId);
            const slight = generateSlight();
            const winnerString = userId === user.id ? "your" : `<@${userId}>'s`;
            return message.reply(`Nice, recorded ${winnerString} win against ${opponent.commonName}, that ${slight}.`);
        },
        None: () => { 
            const slight = generateSlight();
            return message.reply(`You don't appear to be playing this round...${slight}`);
        }


    });
}