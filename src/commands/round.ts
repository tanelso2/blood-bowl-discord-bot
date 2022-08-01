import { assertLeague, CommandContext } from "./core";

export function printRound(context: CommandContext) {
    const { league, formatter, message } = context;
    assertLeague(league);

    const currentRound = league.getCurrentRound(); 
    const roundStatus = formatter.roundStatus(currentRound);
    return message.reply("", { embed: roundStatus });
}