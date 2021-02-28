import { readFileSync } from 'fs';
import * as yaml from 'js-yaml';
import { League, LeagueData } from '@models/league';
import { StringFormat } from './stringFormat';

const leagueFile = './sample-league.yaml';
const data = yaml.load(readFileSync(leagueFile, 'utf-8')) as LeagueData;
const testLeague = new League(data, leagueFile);

describe('StringFormat', () => {
    describe('#coach()', () => {
        const c = testLeague.coaches[0];

        it('should use coach\'s mention string', () => {
            StringFormat.coach(c).should.equal(c.mentionString);
        });
    });

    describe('#game()', () => {
        const g = testLeague.schedule[0].games[0];

        it('should include both coachs\' team types', () => {
            StringFormat.game(g).should
                .contain(g.homeCoach.teamType)
                .contain(g.awayCoach.teamType);
        });
    });

    describe('#round()', () => {
        const round = testLeague.schedule[0];

        it('should include round number in title', () => {
            StringFormat.round(round).should.contain(round.id);
        });
    });

    describe('#league()', () => {
        it('should include league name', () => {
            StringFormat.league(testLeague).should.contain(testLeague.name);
        });
    });
});
