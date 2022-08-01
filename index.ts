import 'module-alias/register';

import * as process from 'process';
import { readFileSync } from 'fs';
import Discord from 'discord.js';
import { logger } from '@core/logger';
import { getLeagueFromFile, League} from '@models/league';
import * as insultGenerator from '@generator/string-generator';
import { ContextError, makeContext } from '@commands/index';
import { CommandContext } from '@commands/core';

const configFile = './config.json';

interface DiscordAuthConfig {
    token: string;
}

const config = JSON.parse(readFileSync(configFile, 'utf-8')) as DiscordAuthConfig;

const LOGGER = logger.child({module:'index'});

const leagueFiles = process.argv.slice(2);
if (leagueFiles.length === 0) {
    console.error('USAGE: node index.js <league_file>...');
    process.exit(1);
}

/**
 *
 * @returns {League[]}
 */
function getAllLeagues(): League[] {
    return leagueFiles.map(getLeagueFromFile);
}

const client = new Discord.Client();

client.once('ready', () => {
    console.log('Ready!');
});

async function handleMessage(message: Discord.Message, client: Discord.Client) {
    const mentionsOptions = {
        "ignoreDirect": false,
        "ignoreRoles": true,
        "ignoreEveryone": true
    };


    /* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */
    if (message.mentions.has(client.user!, mentionsOptions)) {
        // Should only trigger if they mention bot user by name
        //
        await makeContext(client, getAllLeagues(), message).on({
            Left: (e: ContextError) => {
                switch (e.kind) {
                    case "unknown-command": {
                        const insult = insultGenerator.generateString("${insult}");
                        return message.reply(`Dude I have no idea what you're trying to say\n${insult}`);
                    }
                    case "user-in-no-leagues": {
                        return message.reply(`You don't seem to be in any leagues...`);
                    }
                    case "user-in-many-leagues": {
                        const leagueNames: string = e.payload! as string;
                        return message.reply(`Yeah you're going to need to be more specific. You're in these leagues: ${leagueNames}`);
                    }
                }

            },
            Right: async (context: CommandContext) => {
                try {
                    await context.cmd.func(context);
                } catch (e: any) {
                    LOGGER.info(e);
                    return message.channel.send(`Ooff I had a bit of a glitch there`);

                }
            }
        });
    }
}


client.on('message', message => {
    void handleMessage(message, client);
});

console.log("Logging in");
void client.login(config.token);
