/*
 *  Options:
 *      noMention: True|False, default False
 *          Use usernames instead of pinging mentions.
 */
class Format {

    static league(league, options={}) {
        const schedule = this.schedule(league.schedule, options);
        const title = prettyTitle(league.name);
        return `${title}\n\nCurrent round: ${league.currentRound}\n\n${schedule}`;
    }

    static schedule(schedule, options={}) {
        const formatRound = (r) => this.round(r, options);
        return schedule.rounds.map(formatRound).join('\n\n');
    }

    static round(round, options={}) {
        const title = `Round ${round.id}:`;

        const formatGame = (g) => this.game(g, options);
        const games = indent(round.games.map(formatGame).join('\n'));

        return `${title}\n${games}`;
    }

    static game(game, options={}) {
        const home = game.homeCoach;
        const away = game.awayCoach;

        const homeName = this.coach(home, options);
        const awayName = this.coach(away, options);

        return `${homeName} (${home.teamType}) v (${away.teamType}) ${awayName}`;
    }

    static coach(coach, options={}) {
        if ('noMention' in options && options['noMention']) {
            return coach.commonName;
        }
        return coach.mentionString;
    }
}

function prettyTitle(title) {
    const border = '#'.repeat(title.length);
    return [border, title, border].join('\n');
}

function indent(s) {
    const pad = (line) => (line !== '') ? '  '.concat(line) : line;
    return s.split('\n').map(pad).join('\n');
}

module.exports = { Format };
