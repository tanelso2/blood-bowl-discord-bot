import Discord from 'discord.js';
import { DiscordFormat } from '@formatting/discordFormat';
import { League } from '@models/league';

export interface Command {
    name: string;
    func: CommandFunc;
    description: string;
    requiresLeague: boolean;
}

export interface CommandContext {
    cmd: Command;
    client: Discord.Client;
    user: Discord.User;
    message: Discord.Message;
    league: League | undefined;
    leagues: League[];
    formatter: DiscordFormat;
    messageParts: string[];
    restOfMessage: string[];
}

export type CommandFunc = (context: CommandContext) => Promise<any>

export function makeCommand(name: string, func: CommandFunc, description: string, requiresLeague: boolean): Command {
    return { name, func, description, requiresLeague };
}

export function assertLeague(x: League | undefined): asserts x is League {
    if (x === undefined) {
        throw new Error("This command needs a league, but undefined was passed in");
    }
}
