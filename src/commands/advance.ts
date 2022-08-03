import Discord from 'discord.js';
import { logger } from '@core/logger';
import { assertLeague, CommandContext } from './core';
import { generateInsult } from '@generator/helpers';
import { Round } from '@models/round';

const LOGGER = logger.child({module:'commands/advance'});

/**
 * Advances the league round and posts the new round in channel.
 *
 * The new round announcement message will be pinned and any prior announcements will be unpinned.
 *
 * Example output:
 *  @owner, round has been advanced
 *  <embed>
 */
export async function advanceRound(context: CommandContext) {
    const { client, message, league, formatter, user } = context;
    assertLeague(league);

    // Unpins *all* other messages by this bot, but until any other messages are expected to be
    // pinned, this is easier than persisting message ids.
    async function unpinOtherMessages(latestMessage: Discord.Message): Promise<Discord.Message[]> {
        const pinnedMessages = await latestMessage.channel.messages.fetchPinned();
        const otherMessages = pinnedMessages
                .filter((message: Discord.Message) => message.author.id === client.user?.id ?? "")
                .filter(x => x.id !== latestMessage.id);
        return Promise.all(
            otherMessages
                .mapValues((x: Discord.Message) => x.unpin())
                .array()
        );
    }

    if (user.id !== league.ownerId) {
        const insult = generateInsult();
        await message.channel.send(`You're not the fucking owner of this league, ${user.toString()}\n${insult}`);
    } else {
        await league.incrementRound().match({
            Left: (e: Error) => {return void message.reply(e.message)},
            Right: async (newRound: Round) => {
                try {
                    const reply = await message
                        .reply(
                            `round has been advanced.`,
                            {
                                embed: formatter.roundAdvance(newRound),
                                disableMentions: 'all',
                            }
                        );
                    await reply.pin();
                    return await unpinOtherMessages(reply);
                } catch (reason) {
                    LOGGER.error(reason as Error);
                }
            }
        });
    }
}
