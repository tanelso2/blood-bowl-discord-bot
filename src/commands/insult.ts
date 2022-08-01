import * as insultGenerator from '@generator/string-generator';
import { CommandContext } from './core';

export async function printInsult({message}: CommandContext) {
    const insult = insultGenerator.generateString("${insult}");
    await message.reply(insult);
}