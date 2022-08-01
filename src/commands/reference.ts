import { TeamType } from "@models/teamtype";
import { ManagementDB } from "@models/utils/db-wrapper";
import * as stringUtils from '@utils/stringUtils';
import { CommandContext } from "./core";

export async function consultReference(context: CommandContext) {
    const {message, restOfMessage} = context;
    const db = new ManagementDB();
    const referenceLookup = restOfMessage;
    let teamName: string | undefined = undefined;
    let starPlayersMode = false;
    if (referenceLookup.length === 0) {
        return message.reply(`ERROR: can't lookup nothing`);
    } else if (referenceLookup.length === 1) {
        teamName = referenceLookup[0];
        if (referenceLookup[0] === "teams") {
            const allTeams = await TeamType.getAllTeamTypes(db);
            const allNames = allTeams.map(x => x.name).sort();
            return message.reply(`All Teams: ${allNames.join(', ')}`);
        } else if (referenceLookup[0] === "help") {
            const reply = stringUtils.trimMultilineLiteral(`\`\`\`
                reference help
                reference teams
                reference <team>
                reference star players <team>
                \`\`\``);
            return message.reply(reply);
        }
    } else if (referenceLookup[0] === "star" && referenceLookup[1] === "players") {
        starPlayersMode = true;
        teamName = referenceLookup[2];
    } else {
        return message.reply(`ERROR: Couldn't understand what to lookup from ${referenceLookup.join(' ')}`);
    }

    if (!teamName) {
        return message.reply(`ERROR: no teamname found? Shouldn't reach this line anyways`)
    }

    const teamType = await TeamType.getTeamTypeFromName(db, teamName);
    if (starPlayersMode) {
        const refString = await teamType.getStarPlayersReferenceString();
        return message.reply(refString);
    } else {
        const refString = await teamType.getPlayersReferenceString();
        return message.reply(refString);
    }
}