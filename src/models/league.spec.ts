import {mockCoach0, mockCoach1, mockCoach2, mockCoach3, mockLeague} from './utils/mocks'
import {Round, RoundData} from './round'
import {makeTmpFileLeague, getLeagueFromFile, LeagueData} from './league'
import { Game } from './game';

describe('League', () => {
    describe('incrementRound()', () => {
        it('Should increment the round correctly', () => {
            const l = makeTmpFileLeague(mockLeague);
            const startingRound = l.currentRound;
            const startingRoundGames = l.getCurrentRound().unwrap().games;
            l.incrementRound().on({
                Left: (e: Error) => { throw e; },
                Right: (r: Round) => {
                    r.round.should.eql(startingRound+1);
                    r.games.should.not.eql(startingRoundGames);
                    r.games.should.eql(l.getCurrentRound().unwrap().games);
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
        });

        it('should persist winner declarations', () => {
            const l = makeTmpFileLeague(mockLeague);
            l.save();
            const userGame: Game = l.findUserCurrentGame(mockCoach0.id).unwrap();
            userGame.hasWinner().should.not.be.true;
            userGame.declareWinner(mockCoach0.id);
            l.save();
            const l2 = getLeagueFromFile(l.leagueFile);
            const savedGame: Game = l2.findUserCurrentGame(mockCoach0.id).unwrap();
            savedGame.hasWinner().should.be.true;
            savedGame.winner?.should.eql(mockCoach0.teamName);
            // Check to make sure there is still an unmarked game
            l2.getCurrentRound().unwrap().games.filter((x) => !x.hasWinner()).length.should.eql(1);
            l2.incrementRound();
            // Make sure it only effects the user's game in the current round, not the next one
            const nextGame: Game = l2.findUserCurrentGame(mockCoach0.id).unwrap();
            nextGame.hasWinner().should.not.be.true;
        });
    });

    describe('Tournament', () => {
        const firstRound: RoundData = {
            round: 1,
            games: [
                {home: mockCoach0.teamName, away: mockCoach1.teamName},
                {home: mockCoach2.teamName, away: mockCoach3.teamName}
            ]
        };
        const mockTournament: LeagueData = {
            ...mockLeague,
            schedule: [firstRound],
            type: 'tournament',
        };

        it('should not advance until all games have a winner', () => {
            const l = makeTmpFileLeague(mockTournament);
            l.getCurrentRound().isSome().should.be.true;
            l.getCurrentRound().unwrap().round.should.eql(1);
            l.incrementRound().on({
                Left: (e: Error) => {},
                Right: (r: Round) => { throw new Error("Shouldn't reach here")}
            });
            l.getCurrentRound().unwrap().round.should.eql(1);
            l.getCurrentRound().unwrap().games.forEach((g) => g.declareWinner(g.homeCoach.id));
            l.incrementRound().on({
                Left: (e: Error) => {throw new Error("Shouldn't reach here, round should advance");},
                Right: (r: Round) => {}
            })
            l.getCurrentRound().unwrap().round.should.eql(2);
        });

        it('should create new rounds correctly', () => {
            const l = makeTmpFileLeague(mockTournament);
            l.schedule.length.should.eql(1);
            l.getCurrentRound().isSome().should.be.true;
            l.getCurrentRound().unwrap().games.forEach(g => g.declareWinner(g.homeCoach.id));
            l.incrementRound();
            const r = l.getCurrentRound().unwrap();
            r.round.should.eql(2);
            r.games.length.should.eql(1);
            const coaches = r.games[0].coaches;
            coaches.some(c => c.id === mockCoach0.id).should.be.true;
            coaches.some(c => c.id === mockCoach1.id).should.not.be.true;
            coaches.some(c => c.id === mockCoach2.id).should.be.true;
            r.games[0].hasWinner().should.not.be.true;
            l.save();
            const l2 = getLeagueFromFile(l.leagueFile);
            l2.schedule.length.should.eql(2);
        });
        
        it('should not advance past the end of the tournament', () => {
            const l = makeTmpFileLeague(mockTournament);
            l.getCurrentRound().isSome().should.be.true;
            l.getCurrentRound().unwrap().games.forEach(g => g.declareWinner(g.homeCoach.id));
            l.incrementRound();
            l.getCurrentRound().unwrap().games.forEach(g => g.declareWinner(g.homeCoach.id));
            const lastRound = l.getCurrentRound().unwrap().round;
            l.incrementRound().on({
                Left: (e: Error) => {},
                Right: (r: Round) => {throw new Error("Shouldn't be able to increment round");}
            });
            l.getCurrentRound().unwrap().round.should.eql(lastRound);
        });
    })

})