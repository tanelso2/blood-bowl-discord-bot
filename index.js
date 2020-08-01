const process = require('process');
const Discord = require('discord.js');
const config = require('./config.json');
const { DiscordFormat } = require('./formatting/discordFormat.js');
const logger = require('./logger.js').child({ module: 'index' });
const { getLeagueFromFile } = require('./models/league.js');
const insultGenerator = require('./generator/string-generator.js');
const stringUtils = require('./utils/stringUtils.js');


const leagueFile = process.argv[2];
if (!leagueFile) {
    console.error('USAGE: node index.js <league_file>');
    process.exit(1);
}

const client = new Discord.Client();
const formatter = new DiscordFormat(client);

/*
 * Example output:
 *  @owner, round has been advanced
 *  <embed>
 */
function advanceRound(message, user) {
    const league = getLeagueFromFile(leagueFile);
    if (user.id !== league.ownerId) {
        const insult = insultGenerator.generateString("${insult}");
        return message.channel.send(`You're not the fucking owner of this league, ${user}\n${insult}`);
    } else {
        const newRound = league.incrementRound();
        if (newRound == null) {
            return message.reply("I cannae do dat captain!, this is the last rund I knae about!")
        }

        return message.reply(
            `round has been advanced.`,
            {
                embed: formatter.roundAdvance(newRound),
                disableMentions: 'all',
            });
    }
}

/*
 * Example output:
 *  @user you are playing @opponent this round
 */
function findOpponent(message, user) {
    const userInGame = (game) => game.coaches.some((c) => c.id === user.id);
    const usersGame = getLeagueFromFile(leagueFile).getCurrentRound().games.find(userInGame);

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
function printSchedule(message, user) {
    const league = getLeagueFromFile(leagueFile);
    const matches = league.findUserGames(user);
    if (matches.length) {
        const schedule = formatter.usersSchedule(user, matches, league.currentRound);
        const response = `here is your schedule this league:\n${schedule}`;
        return message.reply(response, {disableMentions: 'all'});
    }
    const insult = insultGenerator.generateString("${insult}");
    return message.reply(`you don't seem to be playing this round, smoothbrain.\n${insult}`);
}

function announceGame(message, user) {
    const usersGame = getLeagueFromFile(leagueFile).getCurrentRound().findUserGame(user);


    if (!usersGame) {
        return message.reply("you don't seem to be playing this round, smoothbrain.");
    }

    const [homeCoach, awayCoach] = usersGame.coaches;

    return message.channel.send(`@here - ${homeCoach.teamType} v. ${awayCoach.teamType}`);
}

function printInsult(message) {
    const insult = insultGenerator.generateString("${insult}");
    message.reply(insult);
}

function markGameDone(message, user) {
    const league = getLeagueFromFile(leagueFile);
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

function printRound(message) {
    const league = getLeagueFromFile(leagueFile);
    const currentRound = league.getCurrentRound();
    const roundStatus = formatter.roundStatus(currentRound);
    return message.reply("", { embed: roundStatus });
}

function makeCommand(name, func, description) {
    return { name, func, description };
}

const commands = [
    makeCommand('advance', advanceRound, 'Advance to the next round (only usable by the league owner)'),
    makeCommand('announce', announceGame, 'Announce that your game for this current round is starting'),
    makeCommand('done', markGameDone, 'Mark your game for this round as done'),
    makeCommand('help', listCommands, 'Display this help text'),
    makeCommand('insult', printInsult, 'Just print out an insult'),
    makeCommand('opponent', findOpponent, 'Display and tag your current opponent'),
    makeCommand('round', printRound, 'Print the status of the current round'),
    makeCommand('schedule', printSchedule, 'Display your schedule for this league')
];

function listCommands(message, _user) {
    const commandList = commands.map((c) => `**${c.name}** - ${c.description}`);
    const commandsString = commandList.join('\n');
    return message.reply(`Available commands: \n${commandsString}`);
}

function findCommand(commandName) {
    return commands.find((c) => c.name == commandName);
}

client.once('ready', () => {
    console.log('Ready!');
});


client.on('message', message => {

    const mentionsOptions = {
        "ignoreDirect": false,
        "ignoreRoles": true,
        "ignoreEveryone": true
    }


    if (message.mentions.has(client.user, mentionsOptions)) {
        // Should only trigger if they mention bot user by name
        //
        const command = message.content.split(/ +/)[1].toLowerCase();

        const cmd = findCommand(command);

        if(cmd) {
            cmd.func(message, message.author);
        } else {
            if (command.endsWith('!')) {
                const trimmedCommand = command.slice(0, -1); // cut off the exclamation point
                const bestFit = stringUtils.getSimilarString(trimmedCommand, commands.map(c => c.name));
                if (bestFit) {
                    const bestFitCmd = findCommand(bestFit);
                    bestFitCmd.func(message, message.author);
                    return;
                }
            }
            const insult = insultGenerator.generateString("${insult}");
            message.channel.send(`Dude I have no idea what you're trying to say\n${insult}`);
        }
    }


});

client.login(config.token);

