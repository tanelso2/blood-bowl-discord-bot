const config = require('./config.json');
const fs = require('fs');
const yaml = require('js-yaml');
const Discord = require('discord.js');
const client = new Discord.Client();
const { League } = require('./league.js')

const leagueFile = './sample-league.yaml';

function getLeague() {
    let content = fs.readFileSync('./sample-league.yaml');
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

function getMatchupString(game) {
    const home = game.homeCoach;
    const away = game.awayCoach;

    return `${home.mentionString} (${home.teamType}) v (${away.teamType}) ${away.mentionString}`;
}

function collectMatchupsForRound(round) {
    const league = getLeague();

    const games = league.getCurrentRound().games;

    const matchups = games.map(getMatchupString);

    const full = '\n'.join(matchups);

    return `${league.name} has been advanced to ${round}\n\nHere are the matchups\n\n${full}`
}


function advanceRound(message, user) {
    const league = getLeague();
    if (user.id !== league.ownerId) {
        message.channel.send(`You're not the fucking owner of this league, ${user}`);
    } else {
        const newRound = incrementRound();
        const response = collectMatchupsForRound(newRound);
        message.channel.send(`Alright ${user}, ${response}`);
    }
    /* Example output:
        Alright @owner, ${league.name} has been advanced to ${round}

        Here are the matchups:

        @user1 (teamType) v (teamType) @user2
        @user3 (teamType) v (teamType) @user4
    */
}

function findOpponent(message, user) {
    /* Example output:
        @user is playing @opponent this round
    */
    const league = getLeague();
    for (const game of league.getCurrentRound().games) {
        const coaches = game.coaches;
        if (coaches.map(c => c.id).includes(user.id)) {
            const otherCoach = coaches.filter(c => (c.id !== user.id))[0];
            return message.channel.send(`${user.toString()} is playing ${otherCoach.mentionString}`);
        }
    }

    // Players not in any match this round fall through to be chastised
    message.channel.send(`${user.toString()} you don't seem to be playing this round, smoothbrain.`);
}

function printSchedule(message, user) {
    message.channel.send(`I'm on my break, ${user}`);
    /* Example output:
        @user, here is your schedule:
        
        1. ${opponent.commonName} - ${opponent.teamName} (${opponent.teamType})
        2. etc....
    */
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
