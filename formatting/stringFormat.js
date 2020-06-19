// Currently unused in favor of DiscordFormat, but useful for debugging
class StringFormat {

    static league(league) {
        const formatRound = (r) => this.round(r);
        const schedule = league.schedule.map(formatRound).join('\n\n');

        return `${league.name}\n\nCurrent round: ${league.currentRound}\n\n${schedule}`;
    }

    static round(round) {
        const title = `Round ${round.id}:`;

        const formatGame = (g) => this.game(g);
        const games = round.games.map(formatGame).join('\n');

        return `${title}\n${games}`;
    }

    static game(game) {
        const home = game.homeCoach;
        const away = game.awayCoach;

        const homeName = this.coach(home);
        const awayName = this.coach(away);

        return `${homeName} (${home.teamType}) v (${away.teamType}) ${awayName}`;
    }

    static coach(coach) {
        return coach.mentionString;
    }

    static coachAndTeam(coach) {
        return `${coach.commonName.padEnd(12)} - ${coach.teamName.padEnd(16)} (${coach.teamType})`
    }
}



module.exports = { StringFormat };
