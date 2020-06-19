const config = require('./config.json');
const fs = require('fs');
const yaml = require('js-yaml');
const Discord = require('discord.js');
const client = new Discord.Client();
const logger = require('./logger.js').child({ module: 'index' });
const { League } = require('./league.js');
const { DiscordFormat } = require('./format.js');

const leagueFile = './sample-league.yaml';

function getLeague() {
    let content = fs.readFileSync(leagueFile);
    let data = yaml.safeLoad(content);
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
                embed: DiscordFormat.roundAdvance(newRound),
                disableMentions: 'all',
            })
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
    const response = `you are playing ${DiscordFormat.coach(opponent)} this round.`;
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
        let schedule = DiscordFormat.usersSchedule(user, matches, league.currentRound);
        schedule = DiscordFormat.makeCodeBlock(schedule);

        const response = `here is your schedule this league:\n${schedule}`;
        return message.reply(response, {disableMentions: 'all'});
    }
    return message.reply(`you don't seem to be playing this round, smoothbrain.`);
}

const commands = {
    'advance': advanceRound,
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
        const command = message.content.split(/ +/)[1];

        const func = commands[command];

        if(func) {
            func(message, message.author);
        } else {
            message.channel.send(`Dude I have no idea what you're trying to say`);
        }
    }


});

client.login(config.token);
