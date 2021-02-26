import { League } from '../models/league';
import { Round } from '../models/round';
import { Game } from '../models/game';
import { Coach } from '../models/coach';

// Currently unused in favor of DiscordFormat, but useful for debugging
export class StringFormat {

    static league(league: League): string {
        const formatRound = (r: Round) => this.round(r);
        const schedule = league.schedule.map(formatRound).join('\n\n');

        return `${league.name}\n\nCurrent round: ${league.currentRound}\n\n${schedule}`;
    }

    static round(round: Round): string {
        const title = `Round ${round.id}:`;

        const formatGame = (g: Game) => this.game(g);
        const games = round.games.map(formatGame).join('\n');

        return `${title}\n${games}`;
    }

    static game(game: Game): string {
        const home = game.homeCoach;
        const away = game.awayCoach;

        const homeName = this.coach(home);
        const awayName = this.coach(away);

        return `${homeName} (${home.teamType}) v (${away.teamType}) ${awayName}`;
    }

    static coach(coach: Coach): string {
        return coach.mentionString;
    }

    static coachAndTeam(coach: Coach): string {
        return `${coach.commonName.padEnd(12)} - ${coach.teamName.padEnd(16)} (${coach.teamType})`
    }
}
