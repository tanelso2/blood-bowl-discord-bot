const should = require('should');
const sinon = require('sinon');
require('should-sinon');

const fs = require('fs');
const yaml = require('js-yaml');
const { League } = require('./league.js');
const { Format } = require('./format.js');


const leagueFile = './sample-league.yaml';
const data = yaml.safeLoad(fs.readFileSync(leagueFile));
const testLeague = new League(data);

afterEach(() => {
    // Restore the default sandbox here
    sinon.restore();
});

describe('Format', function () {
    describe('#coach()', function () {
        const c = testLeague.coaches[0];

        it('should use mention by default', function () {
            Format.coach(c).should.equal(c.mentionString);
        });
        it('should use commonName if \'noMention\' option specified', function () {
            Format.coach(c, {'noMention': true}).should.equal(c.commonName);
        });
    });

    describe('#game()', function() {
        const g = testLeague.schedule.rounds[0].games[0];

        it('should forward format options when formatting coaches', function () {
            sinon.spy(Format, "coach");

            const options = {};
            Format.game(g, options);

            Format.coach.should
                .be.calledTwice()
                .be.calledWithExactly(g.homeCoach, options)
                .be.calledWithExactly(g.awayCoach, options);
        });
        it('should include both coachs\' team types', function () {
            Format.game(g).should
                .containEql(g.homeCoach.teamType)
                .containEql(g.awayCoach.teamType);
        });
    });

    describe('#round()', function() {
        const round = testLeague.schedule.rounds[0];

        it('should forward format options when formatting games', function () {
            sinon.spy(Format, "game");

            const options = {};
            Format.round(round, options);

            Format.game.callCount.should.be.exactly(round.games.length);
            for (const game of round.games) {
                Format.game.should.be.calledWithExactly(game, options);
            }
        });

        it('should include round number in title', function () {
            Format.round(round).should.containEql(round.id);
        });
    });

    describe('#league()', function () {
        it('should forward format options when formatting rounds', function () {
            sinon.spy(Format, "round");

            const options = {};
            Format.league(testLeague, options);

            Format.round.callCount.should.be.exactly(testLeague.schedule.rounds.length);
            for (const round of testLeague.schedule.rounds) {
                Format.round.should.be.calledWithExactly(round, options);
            }
        });

        it('should include league name', function () {
            Format.league(testLeague).should.containEql(testLeague.name);
        });
    });
});
