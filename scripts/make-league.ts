#!/usr/bin/env npx ts-node
import 'module-alias/register';

import * as process from 'process';
import * as rd from 'readline';
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';
import { execSync } from 'child_process';

import { CoachData } from '@models/coach';
import { League, LeagueData } from '@models/league';
import { RoundData } from '@models/round';


const readline = rd.createInterface({
  input: process.stdin,
  output: process.stdout
});

async function query(message: string): Promise<string> {
    return new Promise((resolve, _) => {
        readline.question(message + ': ', (answer) => {
            resolve(answer);
        });
    });
}

async function okPrompt(): Promise<boolean> {
    const response = await query("Is this ok? [y/n]");
    switch (response) {
        case 'y':
        case 'Y':
            return true;
        case 'n':
        case 'N':
            return false;
        default:
            console.log(`y or n, it's not that hard! Fucking figure it out`);
            return await okPrompt();
    }
}

async function pickOne(choices: string[]): Promise<string> {
    const sorted = choices.sort();
    sorted.forEach((v, idx) => {
        console.log(`[${idx}] ${v}`);
    });
    const response = await query("Choose one");
    try {
        const responseNum = parseInt(response);
        return choices[responseNum];
    } catch (err) {
        console.log(`Wow you did something wrong you idiot. Try again`);
        return await pickOne(choices);
    }

}

/**
 * Interrupts current script,
 * opens file in $EDITOR,
 * waits for user to exit $EDITOR,
 * then resumes current script
 */
function openFileInEditor(filename: string) {
    const editor = process.env['EDITOR'];
    return execSync(`${editor} ${filename}`, {
        stdio: 'inherit',
        encoding: 'utf-8'
    });
}

/**
 * @param obj: 'a
 * @param toText: 'a -> string
 * @param fromText: string -> 'b
 * @returns: 'b
 */
async function editObjectInEditor<a,b>(
    obj: a,
    toText: (x: a) => string,
    fromText: (x: string) => b
    ): Promise<b> {
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

async function queryCoach(): Promise<CoachData> {
    const name = await query('coach name');
    const nickname = await query('nickname');
    const id = await query('coach id');
    const teamName = await query('team name');
    const teamType = await query('team type');
    const retVal = { name, id, teamName, teamType, nickname };
    console.log(`This team looks like ${JSON.stringify(retVal)}`);
    const ok = await okPrompt();
    if (!ok) {
        console.log(`Oh did you make a mistake? Fucking meatbags. Fine I'll just throw away all the work we just did and start over. All because you fucked up. You piece of shit`);
        return await queryCoach();
    }
    return retVal;
}

const separator = `\n---------\n`;

function roundToText(teams: string[]): string {
    const pairings = [];
    for (let i = 0; i < teams.length; i += 2) {
        pairings.push([teams[i], teams[i+1]]);
    }
    return pairings.map((x) => `${x[0]}\n${x[1]}`).join(separator);
}

type GameMock = string[]; // Just two team names
type RoundMock = GameMock[];

function textToRound(text: string): RoundMock {
    const pairingStrings = text.split(separator);
    return pairingStrings.map(x => x.trim()).map(x => x.split('\n'));
}

async function makeRounds(teamNames: string[]): Promise<RoundMock[]> {
    const rounds = [];
    for (let i = 0; i < teamNames.length-1; i++) {
        console.log(`Round ${i}`);
        await query('Press enter to continue');
        const round = await editObjectInEditor(teamNames, roundToText, textToRound);
        rounds.push(round);
    }
    return rounds;
}

async function queryCoaches(numCoaches: number): Promise<CoachData[]> {
    const coaches = [];
    for (let i = 0; i < numCoaches; i++) {
        console.log(`~~~Coach number ${i}~~~`);
        const coach = await queryCoach();
        coaches.push(coach);
    }
    return coaches;
}

async function pickOwnerId(coaches: CoachData[]): Promise<string> {
    console.log('The owner of the league is');
    const ownerName = await pickOne(coaches.map(x => x.name));
    return coaches.find(x => x.name === ownerName)!.id;
}

async function main() {
    const name = await query('League name');
    const id = await query('League id');
    const numCoaches = parseInt(await query('Number of coaches'));
    const audienceId = await query('The group id of the discord group that should be notified when games start');
    const coaches = await queryCoaches(numCoaches);
    const ownerId = await pickOwnerId(coaches);
    const teamNames = coaches.map(x => x.teamName);
    const rounds = await makeRounds(teamNames);
    const schedule: RoundData[] = rounds.map((round, idx) => {
        const games = round.map((x) => {
            return {home: x[0], away: x[1], done: false};
        });
        return {round: idx+1, games};
    });
    // TODO: ownerId select
    const data: LeagueData = {
        name, id,
        ownerId,
        currentRound: 1,
        audienceId: audienceId || undefined,
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
