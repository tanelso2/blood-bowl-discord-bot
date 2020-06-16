const config = require('./config.json');
const Discord = require('discord.js');
const client = new Discord.Client();

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
        console.log(message.content);
        console.log(message.cleanContent);
        console.log(message.author.id);
    }


});

client.login(config.token);
