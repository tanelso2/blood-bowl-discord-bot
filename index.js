const process = require('process');
const Discord = require('discord.js');
const config = require('../config.json');
const { DiscordFormat } = require('./formatting/discordFormat.js');
const logger = require('./logger.js').logger.child({ module: 'index' });
const { getLeagueFromFile } = require('./models/league.js');
const insultGenerator = require('./generator/string-generator.js');
const stringUtils = require('./utils/stringUtils.js');
const { Option } = require('./utils/types/option.js');

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
function getAllLeagues() {
    return leagueFiles.map(getLeagueFromFile);
}


/**
 * Returns the leagues this user is involved in
 *
 * @param message
 * @param user
 * @returns {League[]}
 */
function getLeagues(message, user) {
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
function advanceRound(message, user, league) {

    // Unpins *all* other messages by this bot, but until any other messages are expected to be
    // pinned, this is easier than persisting message ids.
    function unpinOtherMessages(latestMessage) {
        return latestMessage.channel.messages.fetchPinned()
            .then(pinned_messages =>
                Promise.all(
                    pinned_messages
                        .filter(message => message.author.id === client.user.id)
                        .filter(message => message.id !== latestMessage.id)
                        .mapValues(message => message.unpin())
                )
            );
    }

    if (user.id !== league.ownerId) {
        const insult = insultGenerator.generateString("${insult}");
        return message.channel.send(`You're not the fucking owner of this league, ${user}\n${insult}`);
    } else {
        league.incrementRound().on({
            None: () => message.reply("I cannae do dat captain!, this is the last rund I knae about!"),
            Some: (newRound) => {
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
                        logger.error(reason);
                    });
            }
        });
    }
}

/*
 * Example output:
 *  @user you are playing @opponent this round
 */
function findOpponent(message, user, league) {
    const userInGame = (game) => game.coaches.some((c) => c.id === user.id);
    const usersGame = league.getCurrentRound().games.find(userInGame);

    if (!usersGame) {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
    }

    const opponent = usersGame.getOpponent(user);
    const response = `you are playing ${formatter.coach(opponent)} this round.`;
    return message.reply(response);
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
function printSchedule(message, user, league) {
    const matches = league.findUserGames(user);
    if (matches.length) {
        const schedule = formatter.usersSchedule(user, matches, league.currentRound);
        const response = `here is your schedule this league:\n${schedule}`;
        return message.reply(response, {disableMentions: 'all'});
    }
    const insult = insultGenerator.generateString("${insult}");
    return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
}

function announceGame(message, user, league) {
    const usersGame = league.getCurrentRound().findUserGame(user);


    if (!usersGame) {
        return message.reply("you don't seem to be playing this round, smoothbrain.");
    }

    const audienceString = league.getAudience().on({
        Some: (roleId) => `<@&${roleId}>`,
        None: () => '@here'
    });

    const [homeCoach, awayCoach] = usersGame.coaches;

    return message.channel.send(`${audienceString} - ${homeCoach.teamType} v. ${awayCoach.teamType}`);
}

function printInsult(message, _, __) {
    const insult = insultGenerator.generateString("${insult}");
    message.reply(insult);
}

function markGameDone(message, user, league) {
    const currentRound = league.getCurrentRound();
    const usersGame = currentRound.findUserGame(user);
    if (!usersGame) {
        const insult = insultGenerator.generateString("${insult}");
        return message.reply(`You don't appear to be playing this round...\n${insult}`);
    }
    usersGame.done = true;
    league.save();

    const numUnfinishedGames = currentRound.getUnfinishedGames().length;
    const opponent = usersGame.getOpponent(user);
    return message.reply(`Gotcha. Your game against ${opponent.commonName} has been recorded. There are ${numUnfinishedGames} left in the round.`);
}

function printRound(message, _, league) {
    const currentRound = league.getCurrentRound();
    const roundStatus = formatter.roundStatus(currentRound);
    return message.reply("", { embed: roundStatus });
}

function makeCommand(name, func, description, requiresLeague) {
    return { name, func, description, requiresLeague };
}

function listLeagues(message, _, __) {
    const leagueIds = getAllLeagues().map(x => x.name).join(',\n ');
    return message.channel.send(`These are all the leagues I know about: \n${leagueIds}\n`);
}

const commands = [
    makeCommand('advance', advanceRound, 'Advance to the next round (only usable by the league owner)', true),
    makeCommand('announce', announceGame, 'Announce that your game for this current round is starting', true),
    makeCommand('done', markGameDone, 'Mark your game for this round as done', true),
    makeCommand('help', listCommands, 'Display this help text', false),
    makeCommand('insult', printInsult, 'Just print out an insult', false),
    makeCommand('list', listLeagues, 'List all the leagues the bot knows about', false),
    makeCommand('opponent', findOpponent, 'Display and tag your current opponent', true),
    makeCommand('round', printRound, 'Print the status of the current round', true),
    makeCommand('schedule', printSchedule, 'Display your schedule for this league', true)
];

function listCommands(message, _, __) {
    const commandList = commands.map((c) => `**${c.name}** - ${c.description}`);
    const commandsString = commandList.join('\n');
    return message.reply(`Available commands: \n${commandsString}`);
}

/**
 * @param {String} rawCommandName
 * @returns {Option<Command>}
 */
function findCommand(rawCommandName) {
    function find(commandName) {
        return commands.find((c) => c.name === commandName);
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


client.on('message', message => {

    const mentionsOptions = {
        "ignoreDirect": false,
        "ignoreRoles": true,
        "ignoreEveryone": true
    };


    if (message.mentions.has(client.user, mentionsOptions)) {
        if (VACATION_MODE) {
            message.reply("Hey hey now, don't ask me to do anything, I have playoffs off. It's in my employment contract");
            return;
        }

        // Should only trigger if they mention bot user by name
        //
        const command = message.content.split(/ +/)[1].toLowerCase();


        findCommand(command).on({
            Some: (cmd) => {
                try {
                    if (!cmd.requiresLeague) {
                        return cmd.func(message, message.author);
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
                } catch (e) {
                    logger.info(e);
                    message.channel.send(`Ooff I had a bit of a glitch there`);
                }
            },
            None: () => {
                const insult = insultGenerator.generateString("${insult}");
                message.reply(`Dude I have no idea what you're trying to say\n${insult}`);
            }
        });
    }
});

client.login(config.token);
