const { League } = require('../models/league.js');

const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});

const exec = require('child_process').execSync;
const path = require('path');
const os = require('os');
const fs = require('fs').promises;

async function query(message) {
    return new Promise((resolve, _) => {
        readline.question(message + ': ', (answer) => {
            resolve(answer);
        });
    });
}

async function okPrompt() {
    const response = await query("Is this ok? [y/n]");
    switch (response) {
        case 'y':
        case 'Y':
            return true;
        case 'n':
        case 'N':
            return false;
        default:
            console.log(`I don't understand, try again`);
            return await okPrompt();
    }
}

/**
 * Interrupts current script,
 * opens file in $EDITOR,
 * waits for user to exit $EDITOR,
 * then resumes current script
 */
function openFileInEditor(filename) {
    const editor = process.env['EDITOR'];
    const opts = {
        stdio: 'inherit'
    };
    return exec(`${editor} ${filename}`, opts);
}

/**
 * @param obj: 'a
 * @param toText: 'a -> string
 * @param fromText: string -> 'b
 * @returns: 'b
 */
async function editObjectInEditor(obj, toText, fromText) {
    const text = toText(obj);
    const randomName = `tmp-${Math.floor(Math.random() * Math.floor(1000))}`;
    const tmpFile = path.join(os.tmpdir(), randomName);
    // create file
    // write text to file
    await fs.writeFile(tmpFile, text);
    openFileInEditor(tmpFile);
    // read file
    const result = await fs.readFile(tmpFile, {encoding: 'utf-8'});
    // translate file using `fromText`
    // return
    return fromText(result);
}

async function queryCoach() {
    const name = await query('coach name');
    const nickname = await query('nickname');
    const id = await query('coach id');
    const teamName = await query('team name');
    const teamType = await query('team type');
    const retVal = { name, id, teamName, teamType, nickname };
    console.log(`This team looks like ${JSON.stringify(retVal)}`);
    const ok = await okPrompt();
    if (!ok) {
        console.log(`Ok, let's try inputting that again`);
        return await queryCoach();
    }
    return retVal;
}

const separator = `\n---------\n`;

function roundToText(teams) {
    const pairings = [];
    for (let i = 0; i < teams.length; i += 2) {
        pairings.push([teams[i], teams[i+1]]);
    }
    return pairings.map((x) => `${x[0]}\n${x[1]}`).join(separator);
}

function textToRound(text) {
    const pairingStrings = text.split(separator);
    return pairingStrings.map(x => x.trim()).map(x => x.split('\n'));
}

async function makeRounds(teamNames) {
    const rounds = [];
    for (let i = 0; i < teamNames.length-1; i++) {
        console.log(`Round ${i}`);
        await query('Press enter to continue');
        const round = await editObjectInEditor(teamNames, roundToText, textToRound);
        rounds.push(round);
    }
    return rounds;
}

async function queryCoaches(numCoaches) {
    const coaches = [];
    for (let i = 0; i < numCoaches; i++) {
        console.log(`~~~Coach number ${i}~~~`);
        const coach = await queryCoach();
        coaches.push(coach);
    }
    return coaches;
}

async function main() {
    const name = await query('League name');
    const id = await query('League id');
    const numCoaches = parseInt(await query('Number of coaches'));
    const audienceId = await query('The group id of the discord group that should be notified when games start');
    const coaches = await queryCoaches(numCoaches);
    const teamNames = coaches.map(x => x.teamName);
    const rounds = await makeRounds(teamNames);
    const schedule = rounds.map((round, idx) => {
        const games = round.map((x) => {
            return {home: x[0], away: x[1], done: false};
        });
        return {round: idx+1, games};
    });
    // TODO: ownerId select
    const data = {
        name, id,
        ownerId: 'TODO',
        currentRound: 1,
        audienceId,
        coaches,
        schedule
    };
    const outputFile = 'output.yaml';
    const l = new League(data, outputFile);
    l.save();
    console.log(`League written out to ${outputFile}`);
    readline.close();
}

main();
