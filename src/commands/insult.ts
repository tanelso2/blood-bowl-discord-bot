import { generateInsult } from '@generator/helpers';
import { CommandContext } from './core';

export async function printInsult({message}: CommandContext) {
    const insult = generateInsult();
    await message.reply(insult);
}