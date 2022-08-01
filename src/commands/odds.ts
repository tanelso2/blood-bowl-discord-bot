import { logger } from '@core/logger';
import { parseOddsScenario, findSuccessProbability, buildTree } from '@odds/odds';
import { CommandContext } from './core';

const LOGGER = logger.child({module: 'commands/odds'});

export async function calculateOdds({message, restOfMessage}: CommandContext) {
    const oddsString = restOfMessage.join(' ');
    LOGGER.debug(`Using '${oddsString}' as input to calculator`);
    try {
        const scenario = parseOddsScenario(oddsString);
        const eventTree = buildTree(scenario);
        const prob = findSuccessProbability(eventTree);
        const reply = `Parsed as ${JSON.stringify(scenario)}\nThe probability is ${prob}`;
        return message.reply(reply);
    } catch (e) {
        return message.reply(`ERROR: ${e as string}`);
    }
}