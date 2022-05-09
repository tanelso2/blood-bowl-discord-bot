import 'module-alias/register';

import * as process from 'process';
import { readFileSync } from 'fs';
import Discord from 'discord.js';
import { DiscordFormat } from '@formatting/discordFormat';
import { logger } from '@core/logger';
import { getLeagueFromFile, League} from '@models/league';
import { Game } from '@models/game';
import * as insultGenerator from '@generator/string-generator';
import * as stringUtils from '@utils/stringUtils';
import { Option } from '@core/types/option';
import { parseOddsScenario, findSuccessProbability, buildTree } from '@odds/odds';

const configFile = './config.json';

interface DiscordAuthConfig {
    token: string;
}

const config: DiscordAuthConfig = JSON.parse(readFileSync(configFile, 'utf-8'));

const LOGGER = logger.child({module:'index'});

const VACATION_MODE = false;

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


/**
 * Returns the leagues this user is involved in
 *
 * @param message
 * @param user
 * @returns {League[]}
 */
function getLeagues(message: Discord.Message, user: Discord.User): League[] {
    const leagueSpecifier = message.content.split(/ +/)[2];
    // TODO: Check if leagueSpecifier is one the bot knows about
    return getAllLeagues()
        .filter(l => l.userInvolvedInLeague(user))
        .filter(l => !leagueSpecifier || l.matches(leagueSpecifier));
}

const client = new Discord.Client();
const formatter = new DiscordFormat(client);

/**
 * Advances the league round and posts the new round in channel.
 *
 * The new round announcement message will be pinned and any prior announcements will be unpinned.
 *
 * Example output:
 *  @owner, round has been advanced
 *  <embed>
 */
async function advanceRound(message: Discord.Message, user: Discord.User, league: League) {

    // Unpins *all* other messages by this bot, but until any other messages are expected to be
    // pinned, this is easier than persisting message ids.
    function unpinOtherMessages(latestMessage: Discord.Message): Promise<Discord.Message[]> {
        return latestMessage.channel.messages.fetchPinned()
            .then(pinned_messages => Promise.all(
                    pinned_messages
                        .filter((message: Discord.Message) => message.author.id === client.user?.id ?? "")
                        .filter(message => message.id !== latestMessage.id)
                        .mapValues((message: Discord.Message) => message.unpin())
                        .array()
            ));
    }

    if (user.id !== league.ownerId) {
        const insult = insultGenerator.generateString("${insult}");
        message.channel.send(`You're not the fucking owner of this league, ${user}\n${insult}`);
    } else {
        league.incrementRound().on<void>({
            Left: (e: Error) => {message.reply(e.message)},
            Right: (newRound) => {
                message
                    .reply(
                        `round has been advanced.`,
                        {
                            embed: formatter.roundAdvance(newRound),
                            disableMentions: 'all',
                        }
                    )
                    .then(reply => reply.pin())
                    .then(reply => unpinOtherMessages(reply))
                    .catch(reason => {
                        LOGGER.error(reason);
                    });
            }
        });
    }
}

/*
 * Example output:
 *  @user you are playing @opponent this round
 */
async function findOpponent(message: Discord.Message, user: Discord.User, league: League) {
    const userInGame = (game: Game) => game.coaches.some((c) => c.id === user.id);
    const usersGame = league.getCurrentRound().games.find(userInGame);

    if (usersGame === undefined) {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
    }

    const opponent = usersGame.getOpponent(user);
    return message.reply(`you are playing ${formatter.coach(opponent)} this round.`);
}

/*
 * Example output:
 *  @user, here is your schedule this league:
 *  ```
 *  1. ${opponent.commonName}     - ${opponent.teamName}     (${opponent.teamType})
 * >2. ${nextOpponent.commonName} - ${nextOpponent.teamName} (${nextOpponent.teamType})
 *  3. etc....
 *  ```
 */
async function printSchedule(message: Discord.Message, user: Discord.User, league: League) {
    const matches = league.findUserGames(user);
    if (matches.length) {
        const schedule = formatter.usersSchedule(user, matches, league.currentRound);
        const response = `here is your schedule this league:\n${schedule}`;
        return message.reply(response, {disableMentions: 'all'});
    }
    const insult = insultGenerator.generateString("${insult}");
    return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
}

async function announceGame(message: Discord.Message, user: Discord.User, league: League) {
    const usersGame = league.getCurrentRound().findUserGame(user);
    return usersGame.on({
        Some: (game: Game) => {
            const audienceString = league.getAudience().on({
                Some: (roleId) => `<@&${roleId}>`,
                None: () => '@here'
            });
            const [homeCoach, awayCoach] = game.coaches;

            return message.channel.send(`${audienceString} - ${homeCoach.teamType} v. ${awayCoach.teamType}`);
        },
        None: () => message.reply("you don't seem to be playing this round, smoothbrain."),
    });
}

async function printInsult(message: Discord.Message, _: Discord.User, __: League) {
    LOGGER.debug(`printInsult called`);
    const insult = insultGenerator.generateString("${insult}");
    message.reply(insult);
}

async function markGameDone(message: Discord.Message, user: Discord.User, league: League) {
    function markDone(finishedGame: Game) {
        finishedGame.done = true;
        league.save();
    }

    function respond(finishedGame: Game) {
        const numUnfinishedGames = currentRound.getUnfinishedGames().length;
        const opponent = finishedGame.getOpponent(user);
        const isOrAre = numUnfinishedGames === 1 ? "is" : "are";
        return message.reply(`Gotcha. Your game against ${opponent.commonName} has been recorded. ` +
                             `There ${isOrAre} ${numUnfinishedGames} left in the round.`);
    }

    function insult() {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`You don't appear to be playing this round...\n${insult}`);
    }

    const currentRound = league.getCurrentRound();
    return currentRound.findUserGame(user).on({
        Some: (game) => {
            markDone(game);
            return respond(game);
        },
        None: () => insult(),
    });
}

function declareWinner(message: Discord.Message, user: Discord.User, league: League) {
    const currentRound = league.getCurrentRound();
    return currentRound.findUserGame(user).on({
        Some: (finishedGame) => {
            finishedGame.declareWinner(user);
            league.save();
            const opponent = finishedGame.unwrap().getOpponent(user);
            const slight = insultGenerator.generateString("${slight}");
            return message.reply(`Nice, recorded your win against ${opponent.commonName}, that ${slight}.`);
        },
        None: () => message.reply(insultGenerator.generateString("You don't appear to be playing this round...${slight}")),
    });
}

function printRound(message: Discord.Message, _: Discord.User, league: League) {
    const currentRound = league.getCurrentRound(); const roundStatus = formatter.roundStatus(currentRound);
    return message.reply("", { embed: roundStatus });
}

type CommandFunc = (message: Discord.Message, user: Discord.User, league: League) => Promise<any>;

interface Command {
    name: string;
    func: CommandFunc;
    description: string;
    requiresLeague: boolean;
}

function makeCommand(name: string, func: CommandFunc, description: string, requiresLeague: boolean): Command {
    return { name, func, description, requiresLeague };
}

function listLeagues(message: Discord.Message, _: Discord.User, __: League) {
    const leagueIds = getAllLeagues().map(x => x.name).join(',\n ');
    return message.channel.send(`These are all the leagues I know about: \n${leagueIds}\n`);
}


async function calculateOdds(message: Discord.Message, _: Discord.User, __: League) {
    const oddsString = message.toString().split(' ').slice(2).join(' ');
    LOGGER.debug(`Using '${oddsString}' as input to calculator`);
    try {
        const scenario = parseOddsScenario(oddsString);
        const eventTree = buildTree(scenario);
        const prob = findSuccessProbability(eventTree);
        const reply = `Parsed as ${JSON.stringify(scenario)}\nThe probability is ${prob}`;
        return message.reply(reply);
    } catch (e) {
        return message.reply(`ERROR: ${e}`);
    }
}

const commands: Command[] = [
    makeCommand('advance', advanceRound, 'Advance to the next round (only usable by the league owner)', true),
    makeCommand('announce', announceGame, 'Announce that your game for this current round is starting', true),
    makeCommand('done', markGameDone, 'Mark your game for this round as done', true),
    makeCommand('help', listCommands, 'Display this help text', false),
    makeCommand('insult', printInsult, 'Just print out an insult', false),
    makeCommand('list', listLeagues, 'List all the leagues the bot knows about', false),
    makeCommand('opponent', findOpponent, 'Display and tag your current opponent', true),
    makeCommand('round', printRound, 'Print the status of the current round', true),
    makeCommand('schedule', printSchedule, 'Display your schedule for this league', true),
    makeCommand('odds', calculateOdds, 'Calculate the odds of an event', false),
    makeCommand('winner', declareWinner, 'Declare yourself the winner of your game this round', true),
];

function listCommands(message:Discord.Message, _: Discord.User, __: League) {
    const commandList = commands.map((c) => `**${c.name}** - ${c.description}`);
    const commandsString = commandList.join('\n');
    return message.reply(`Available commands: \n${commandsString}`);
}

/**
 * @param {String} rawCommandName
 * @returns {Option<Command>}
 */
function findCommand(rawCommandName: string): Option<Command> {
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

client.once('ready', () => {
    console.log('Ready!');
});

async function handleMessage(message: Discord.Message) {
    const mentionsOptions = {
        "ignoreDirect": false,
        "ignoreRoles": true,
        "ignoreEveryone": true
    };


    /* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */
    if (message.mentions.has(client.user!, mentionsOptions)) {
        if (VACATION_MODE) {
            message.reply("Hey hey now, don't ask me to do anything, I have playoffs off. It's in my employment contract");
            return;
        }

        // Should only trigger if they mention bot user by name
        //
        const command = message.content.split(/ +/)[1].toLowerCase();


        findCommand(command).on({
            Some: (cmd: Command) => {
                try {
                    if (!cmd.requiresLeague) {
                        // Linter didn't understand that these types overlap, so convert to unknown first to please it
                        const func = cmd.func as unknown as ((message: Discord.Message, user: Discord.User) => void);
                        return func(message, message.author);
                    }
                    const leagues = getLeagues(message, message.author);
                    if (leagues.length === 0) {
                        message.reply(`You don't seem to be in any leagues...`);
                    } else if (leagues.length > 1) {
                        const leagueNames = leagues.map(x => x.name).join(', ');
                        message.reply(`Yeah you're going to need to be more specific. You're in these leagues: ${leagueNames}`);
                    } else {
                        // Only in one league, must be the one
                        cmd.func(message, message.author, leagues[0]);
                    }
                } catch (e: any) {
                    LOGGER.info(e);
                    message.channel.send(`Ooff I had a bit of a glitch there`);
                }
            },
            None: () => {
                const insult = insultGenerator.generateString("${insult}");
                message.reply(`Dude I have no idea what you're trying to say\n${insult}`);
            }
        });
    }

}


client.on('message', message => {
    handleMessage(message);
});

client.login(config.token);
