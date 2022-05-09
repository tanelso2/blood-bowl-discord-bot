import {mockLeague} from './utils/mocks'
import {Round, RoundData} from './round'
import {League, LeagueData, makeTmpFileLeague, getLeagueFromFile} from './league'

describe('League', () => {
    describe('incrementRound()', () => {
        it('Should increment the round correctly', () => {
            const l = makeTmpFileLeague(mockLeague);
            const startingRound = l.currentRound;
            const startingRoundGames = l.getCurrentRound().games;
            l.incrementRound().on({
                Left: (e: Error) => { throw e; },
                Right: (r: Round) => {
                    r.round.should.eql(startingRound+1);
                    r.games.should.not.eql(startingRoundGames);
                }
            })
        });
    });

    describe('save()', () => {
        it('Should save and load properly', () => {
            const l = makeTmpFileLeague(mockLeague);
            l.save();
            const l2 = getLeagueFromFile(l.leagueFile);
            l.id.should.eql(l2.id);
            l.coaches.length.should.eql(l2.coaches.length);
            l.name.should.eql(l2.name);
        });

        it('should persist round changes', () => {
            const l = makeTmpFileLeague(mockLeague);
            l.save();
            const currRound = getLeagueFromFile(l.leagueFile).currentRound;
            l.incrementRound();
            l.save();
            const newRound = getLeagueFromFile(l.leagueFile).currentRound;
            newRound.should.eql(currRound+1);
        })
    });

})