const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const fs = require('fs');
const yaml = require('js-yaml');
const { League } = require('../models/league.js');
const { StringFormat } = require('./stringFormat.js');


const leagueFile = './sample-league.yaml';
const data = yaml.safeLoad(fs.readFileSync(leagueFile));
const testLeague = new League(data);

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

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
                .containEql(g.homeCoach.teamType)
                .containEql(g.awayCoach.teamType);
        });
    });

    describe('#round()', () => {
        const round = testLeague.schedule[0];

        it('should include round number in title', () => {
            StringFormat.round(round).should.containEql(round.id);
        });
    });

    describe('#league()', () => {
        it('should include league name', () => {
            StringFormat.league(testLeague).should.containEql(testLeague.name);
        });
    });
});
