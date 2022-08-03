import { Command, makeCommand, CommandContext } from "./core";
import Discord from 'discord.js';
import * as stringUtils from '@utils/stringUtils';
import { printInsult } from "./insult";
import { advanceRound } from "./advance";
import { announceGame } from "./announce";
import { markGameDone } from "./done";
import { findOpponent } from "./opponent";
import { consultReference } from "./reference";
import { printRound } from "./round";
import { printSchedule } from "./schedule";
import { calculateOdds } from "./odds";
import { declareWinner } from "./winner";
import { Option } from "@core/types/generated/option";
import { DiscordFormat } from "@formatting/discordFormat";
import { League } from "@models/league";
import { Either } from "@core/types/generated/either";

const commands: Command[] = [
    makeCommand('advance', advanceRound, 'Advance to the next round (only usable by the league owner)', true),
    makeCommand('announce', announceGame, 'Announce that your game for this current round is starting', true),
    makeCommand('done', markGameDone, 'Mark your game for this round as done', true),
    makeCommand('help', listCommands, 'Display this help text', false),
    makeCommand('insult', printInsult, 'Just print out an insult', false),
    makeCommand('list', listLeagues, 'List all the leagues the bot knows about', false),
    makeCommand('opponent', findOpponent, 'Display and tag your current opponent', true),
    makeCommand('reference', consultReference, 'Display information about team types', false),
    makeCommand('round', printRound, 'Print the status of the current round', true),
    makeCommand('schedule', printSchedule, 'Display your schedule for this league', true),
    makeCommand('odds', calculateOdds, 'Calculate the odds of an event', false),
    makeCommand('winner', declareWinner, 'Declare yourself the winner of your game this round', true),
];

function listLeagues(context: CommandContext) {
    const { leagues, message } = context;
    const leagueIds = leagues.map(x => x.name).join(',\n ');
    return message.channel.send(`These are all the leagues I know about: \n${leagueIds}\n`);
}

function listCommands(context: CommandContext) {
    const { message } = context;
    const commandList = commands.map((c) => `**${c.name}** - ${c.description}`);
    const commandsString = commandList.join('\n');
    return message.reply(`Available commands: \n${commandsString}`);
}

/**
 * @param {String} rawCommandName
 * @returns {Option<Command>}
 */
export function findCommand(rawCommandName: string): Option<Command> {
    function find(commandName: string) {
        return commands.find((c: Command) => c.name === commandName);
    }

    const exactMatch = find(rawCommandName);
    if (exactMatch) {
        return Option.Some(exactMatch);
    } else if (!exactMatch && rawCommandName.endsWith('!')) {
        const trimmedCommand = rawCommandName.slice(0, -1); // cut off the exclamation point
        const bestFit = stringUtils.getSimilarString(trimmedCommand, commands.map(c => c.name));
        if (bestFit) {
            const fitMatch = find(bestFit);
            if (fitMatch) {
                return Option.Some(fitMatch);
            }
        }
    }

    return Option.None();
}

/**
 * Returns the leagues this user is involved in
 *
 * @param message
 * @param user
 * @returns {League[]}
 */
function filterLeagues(message: Discord.Message, user: Discord.User, leagues: League[]): League[] {
    return leagues 
        .filter(l => l.userInvolvedInLeague(user))
}

export type ContextErrorKind = "unknown-command" | "user-in-no-leagues" | "user-in-many-leagues"
export type ContextError = {kind: ContextErrorKind, payload?: any}

export function makeContext(client: Discord.Client, leagues: League[], message: Discord.Message): Either<ContextError, CommandContext> {
    const user = message.author;
    
    const formatter = new DiscordFormat(client);
    const messageParts = message.toString().split(/\s/);

    const rawCommandName = messageParts[1].toLowerCase();
    return findCommand(rawCommandName).match<Either<ContextError, CommandContext>>({
        None: () => Either.Left({kind: "unknown-command"}),
        Some: (cmd: Command) => {
            let partsUsed = 2;
            let league = undefined;
            if (cmd.requiresLeague) {
                const userLeagues = filterLeagues(message, message.author, leagues);
                if (userLeagues.length === 0) {
                    return Either.Left({kind: "user-in-no-leagues"})
                } else if (userLeagues.length > 1) {
                    const leagueNames = userLeagues.map(x => x.name).join(', ');
                    let found = false;
                    if (messageParts.length > 2) {
                        const leagueSpecifier = messageParts[2];
                        const l = userLeagues.find(x => x.matches(leagueSpecifier));
                        if (l) {
                            league = l;
                            partsUsed += 1;
                            found = true;
                        }
                    }

                    if (!found) {
                        return Either.Left({kind: "user-in-many-leagues", payload: leagueNames})
                    }
                } else {
                    // Only in one league, must be the one
                    league = leagues[0];
                }
            }
            return Either.Right({
                cmd, client, user, message, league, leagues, formatter, messageParts,
                restOfMessage: messageParts.slice(partsUsed)
            });
        }
    });

}