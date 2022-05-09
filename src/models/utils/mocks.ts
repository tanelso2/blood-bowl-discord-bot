import { CoachData } from '../coach';
import { LeagueData } from '../league'
import {RoundData} from '../round'

export const mockCoach0 = {
    id: '0',
    name: 'Coach 0',
    teamName: '0ville 0s',
    teamType: 'Chaos',
};

export const mockCoach1 = {
    id: '1',
    name: 'Coach 1',
    teamName: 'San Uno Unidos',
    teamType: 'Human',
};

export const mockCoach2 = {
    id: '2',
    name: 'Coach 2',
    teamName: 'Two York City Halflings',
    teamType: 'Halfling',
};

export const mockCoach3 = {
    id: '3',
    name: 'Coach 3',
    teamName: '3 town boiz',
    teamType: 'Ork',
};


export const mockCoaches: CoachData[] = [
    mockCoach0,
    mockCoach1,
    mockCoach2,
    mockCoach3
];

export const mockSchedule: RoundData[] = [
    {
        round: 1, 
        games: [
            {
                home: mockCoach0.teamName, 
                away: mockCoach1.teamName
            },
            {
                home: mockCoach2.teamName,
                away: mockCoach3.teamName
            }
        ]
    }, {
        round: 2,
        games: [
            {
                home: mockCoach0.teamName,
                away: mockCoach2.teamName
            },
            {
                home: mockCoach1.teamName,
                away: mockCoach3.teamName
            }
        ]
    }, {
        round: 3,
        games: [
            {
                home: mockCoach0.teamName,
                away: mockCoach3.teamName
            },
            {
                home: mockCoach1.teamName,
                away: mockCoach2.teamName
            }
        ]
    }
];


export const mockLeague: LeagueData = {
    name: 'mock',
    id: 'mock',
    ownerId: mockCoach0.id,
    currentRound: 1,
    type: 'round-robin',
    coaches: mockCoaches,
    schedule: mockSchedule
};