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

        it('should use mention by default', function () {
            StringFormat.coach(c).should.equal(c.mentionString);
        });
        it('should use commonName if \'noMention\' option specified', function () {
            StringFormat.coach(c, {'noMention': true}).should.equal(c.commonName);
        });
    });

    describe('#game()', function() {
        const g = testLeague.schedule[0].games[0];

        it('should forward format options when formatting coaches', function () {
            sinon.spy(StringFormat, "coach");

            const options = {};
            StringFormat.game(g, options);

            StringFormat.coach.should
                .be.calledTwice()
                .be.calledWithExactly(g.homeCoach, options)
                .be.calledWithExactly(g.awayCoach, options);
        });
        it('should include both coachs\' team types', function () {
            StringFormat.game(g).should
                .containEql(g.homeCoach.teamType)
                .containEql(g.awayCoach.teamType);
        });
    });

    describe('#round()', function() {
        const round = testLeague.schedule[0];

        it('should forward format options when formatting games', function () {
            sinon.spy(StringFormat, "game");

            const options = {};
            StringFormat.round(round, options);

            StringFormat.game.callCount.should.be.exactly(round.games.length);
            for (const game of round.games) {
                StringFormat.game.should.be.calledWithExactly(game, options);
            }
        });

        it('should include round number in title', function () {
            StringFormat.round(round).should.containEql(round.id);
        });
    });

    describe('#league()', function () {
        it('should forward format options when formatting rounds', function () {
            sinon.spy(StringFormat, "round");

            const options = {};
            StringFormat.league(testLeague, options);

            StringFormat.round.callCount.should.be.exactly(testLeague.schedule.length);
            for (const round of testLeague.schedule) {
                StringFormat.round.should.be.calledWithExactly(round, options);
            }
        });

        it('should include league name', function () {
            StringFormat.league(testLeague).should.containEql(testLeague.name);
        });
    });
});
