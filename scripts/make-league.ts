#!/usr/bin/env npx ts-node
import 'module-alias/register';

import * as process from 'process';
import * as rd from 'readline';
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';
import { execSync } from 'child_process';

import { CoachData } from '@models/coach';
import { getLeagueFromFile, League, LeagueData, TournamentSeason } from '@models/league';
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
    let editor = process.env['EDITOR'];
    if (!editor) {
        throw new Error(`User has no EDITOR env variable set`);
    }
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
    console.log('\n');
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
        console.log(`Round ${i+1}`);
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
    return coaches.find(x => x.name === ownerName)?.id ?? "";
}

async function queryLeagueData(): Promise<LeagueData> {
    const name = await query('League name');
    const id = await query('League id');
    const type = await pickOne(['round-robin', 'tournament']);
    const numCoaches = parseInt(await query('Number of coaches'));
    const audienceId = await query('The group id of the discord group that should be notified when games start');
    const coaches = await queryCoaches(numCoaches);
    const ownerId = await pickOwnerId(coaches);
    return {
        name, id, ownerId,
        audienceId: audienceId || undefined,
        type,
        currentRound: 0,
        coaches,
        schedule: []
    };
}

function copyLeagueData(sourceFile: string): LeagueData {
    return getLeagueFromFile(sourceFile);
}

async function makeRankings(coaches: CoachData[]): Promise<CoachData[]> {
    const teamNames = coaches.map(c => c.teamName);
    const teamNamesRanked = await editObjectInEditor(teamNames, 
        (x) => x.join('\n'), 
        (x) => x.trim().split('\n'))
    const coachesRanked = teamNamesRanked
        .map((teamName) => coaches.find((x) => x.teamName === teamName)!)
    return coachesRanked;
}

async function main(): Promise<void> {
    const args = process.argv;
    let sourceFile = null;
    let tournament = false;
    for (let i = 1; i < args.length; i++) {
       const curr = args[i]; 
       switch (curr) {
            case '--copy':
                // use the next argument
                sourceFile = args[++i];
                break;
            case '--playoffs':
                sourceFile = args[++i];
                tournament = true;
                break;
       }
    }
    let leagueData;
    if (sourceFile) {
        leagueData = copyLeagueData(sourceFile);
    } else {
        leagueData = await queryLeagueData();
    }
    if (tournament) {
        leagueData.type = 'tournament';
    }
    if (leagueData.type === 'round-robin') {
        const teamNames = leagueData.coaches.map(x => x.teamName);
        const rounds = await makeRounds(teamNames);
        const schedule: RoundData[] = rounds.map((round, idx) => {
            const games = round.map((x) => {
                return {home: x[0], away: x[1]};
            });
            return {round: idx+1, games};
        });
        leagueData.schedule = schedule;
        leagueData.currentRound = 0;
    } else if (leagueData.type === 'tournament') {
        const rankings = await makeRankings(leagueData.coaches);
        const round = TournamentSeason.makeFirstRoundGivenRankings(rankings);
        leagueData.schedule = [round];
        leagueData.currentRound = 0;
    }
    const outputFile = 'output.yaml';
    const l = new League(leagueData, outputFile);
    l.save();
    console.log(`League written out to ${outputFile}`);
    readline.close();
}

// void needed to mark the async Promise as ignored
void main();
