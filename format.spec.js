const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const fs = require('fs');
const yaml = require('js-yaml');
const { League } = require('./league.js');
const { StringFormat } = require('./format.js');


const leagueFile = './sample-league.yaml';
const data = yaml.safeLoad(fs.readFileSync(leagueFile));
const testLeague = new League(data);

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('StringFormat', function () {
    describe('#coach()', function () {
        const c = testLeague.coaches[0];

        it('should use coach\'s mention string', function () {
            StringFormat.coach(c).should.equal(c.mentionString);
        });
    });

    describe('#game()', function() {
        const g = testLeague.schedule[0].games[0];

        it('should include both coachs\' team types', function () {
            StringFormat.game(g).should
                .containEql(g.homeCoach.teamType)
                .containEql(g.awayCoach.teamType);
        });
    });

    describe('#round()', function() {
        const round = testLeague.schedule[0];

        it('should include round number in title', function () {
            StringFormat.round(round).should.containEql(round.id);
        });
    });

    describe('#league()', function () {
        it('should include league name', function () {
            StringFormat.league(testLeague).should.containEql(testLeague.name);
        });
    });
});
