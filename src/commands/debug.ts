import { CommandContext } from "./core";
import * as fs from 'fs/promises';

export async function debug(context: CommandContext) {
    const { restOfMessage, leagues, message } = context;
    if (restOfMessage[0] === 'yaml') {
        const fileContents = leagues.map(async (l) => {
            const content = await fs.readFile(l.leagueFile);
            const formattedContent = "```yaml\n" + content.toString('utf-8') + "\n```";
            return `\`${l.leagueFile}\`:\n${formattedContent}`;
        });
        const fileStrings = await Promise.all(fileContents);
        const reply: string = fileStrings.join('\n');
        return message.reply(reply);
    }
}