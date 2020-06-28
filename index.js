const fs = require('fs');
const yaml = require('js-yaml');
const process = require('process');
const Discord = require('discord.js');
const config = require('./config.json');
const { DiscordFormat } = require('./formatting/discordFormat.js');
const logger = require('./logger.js').child({ module: 'index' });
const { League } = require('./models/league.js');


const leagueFile = process.argv[2];
if (!leagueFile) {
    console.error('USAGE: node index.js <league_file>');
    process.exit(1);
}

const client = new Discord.Client();
const formatter = new DiscordFormat(client);

function getLeague() {
    const content = fs.readFileSync(leagueFile);
    const data = yaml.safeLoad(content);
    return new League(data);
}

function incrementRound() {
    const content = fs.readFileSync(leagueFile);
    const data = yaml.safeLoad(content);

    const currentRound = data.currentRound;

    const newRound = currentRound + 1;

    data.currentRound = newRound;

    const yamlStr = yaml.safeDump(data);
    fs.writeFileSync(leagueFile, yamlStr, 'utf8');

    return newRound;
}

/*
 * Example output:
 *  @owner, round has been advanced
 *  <embed>
 */
function advanceRound(message, user) {
    const league = getLeague();
    if (user.id !== league.ownerId) {
        return message.channel.send(`You're not the fucking owner of this league, ${user}`);
    } else {
        incrementRound();
        const newRound = getLeague().getCurrentRound();

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
    const usersGame = getLeague().getCurrentRound().games.find(userInGame);

    if (!usersGame) {
        return message.reply("you don't seem to be playing this round, smoothbrain.");
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
    const league = getLeague();
    const matches = league.findUserGames(user);
    if (matches.length) {
        const schedule = formatter.usersSchedule(user, matches, league.currentRound);
        const response = `here is your schedule this league:\n${schedule}`;
        return message.reply(response, {disableMentions: 'all'});
    }
    return message.reply(`you don't seem to be playing this round, smoothbrain.`);
}

function announceGame(message, user) {
    const league = getLeague();
    const userInGame = (game) => game.coaches.some((c) => c.id === user.id);
    const usersGame = getLeague().getCurrentRound().games.find(userInGame);


    if (!usersGame) {
        return message.reply("you don't seem to be playing this round, smoothbrain.");
    }

    const [homeCoach, awayCoach] = usersGame.coaches;

    // I don't actually know how to do @here mentions from the bot, let's play around with
    // it until it works
    return message.reply(`<@here> - ${homeCoach.teamType} v. ${awayCoach.teamType}`);
}

const commands = {
    'advance': advanceRound,
    'announce': announceGame,
    'opponent': findOpponent,
    'schedule': printSchedule
};

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

        const func = commands[command];

        if(func) {
            func(message, message.author);
        } else {
            message.channel.send(`Dude I have no idea what you're trying to say`);
        }
    }


});

client.login(config.token);
