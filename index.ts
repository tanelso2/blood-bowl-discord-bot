import 'module-alias/register';

import { readFileSync } from 'fs';
import { Client, Events, GatewayIntentBits, Message } from 'discord.js';
import { logger } from '@core/logger';

const configFile = './config.json';

interface DiscordAuthConfig {
    token: string;
}

const config = JSON.parse(readFileSync(configFile, 'utf-8')) as DiscordAuthConfig;

const LOGGER = logger.child({module:'index'});


const intents = [
    GatewayIntentBits.Guilds,
    GatewayIntentBits.GuildMessages,
    GatewayIntentBits.DirectMessages,
    GatewayIntentBits.MessageContent
];

const client = new Client({intents});

client.once(Events.ClientReady, (_) => {
    console.log('Ready!');
});

async function handleMessage(message: Message, client: Client) {
    const mentionsOptions = {
        ignoreDirect: false,
        ignoreRoles: true,
        ignoreEveryone: true
    };


    /* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */
    if (message.mentions.has(client.user!, mentionsOptions)) {
        // Should only trigger if they mention bot user by name
        //
        LOGGER.info(`Someone talked to the bot`);

    }
}

client.on(Events.Error, e => {
    console.error(`Got an error: ${e.message}`);
});


client.on(Events.MessageCreate, message => {
    void handleMessage(message, client);
});

console.log("Logging in");
void client.login(config.token);
