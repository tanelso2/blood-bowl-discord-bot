import { logger } from '@core/logger';
import { Coach } from './coach';
import { Option } from '@core/types/generated/option';
import Discord from 'discord.js';

const LOGGER = logger.child({module: 'game'});

export interface GameData {
    home: string;
    away: string;
    done?: boolean;
    winner?: string;
}

/** One match up in a Round. */
export class Game implements GameData {
    home: string;
    away: string;
    done: boolean;
    homeCoach: Coach;
    awayCoach: Coach;
    winner?: string;

    constructor(data: GameData, coaches: Coach[]) {
        this.home = data.home;
        this.away = data.away;
        this.done = data.done || false; // false if not-exists
        this.homeCoach = coaches.find(c => c.teamNameIsCloseEnough(data.home)) || Coach.null();
        this.awayCoach = coaches.find(c => c.teamNameIsCloseEnough(data.away)) || Coach.null();
        this.winner = data.winner;
    }

    /** @member {Array<Coach>} - Both coaches playing in this game. Order not guaranteed. Results may vary */
    get coaches(): Coach[] {
        return [this.homeCoach, this.awayCoach];
    }

    /**
     * Gets the opposing coach for this game.
     *
     * @param {Discord.User} user
     * @return {Coach}
     */
    getOpponent(user: Discord.User): Coach {
        const opponent = Option.ofNullable(this.coaches.find((c) => c.id !== user.id));
        return opponent.on({
            Some: (opponent: Coach) => opponent,
            None: () => {
                LOGGER.error(`unknown opponent of ${user.id}`);
                throw new Error(`unknown opponent of ${user.id}`);
            },
        });
    }

    hasWinner(): boolean {
        return !!this.winner
    }

    /**
     * @param {string} userId - The winner of the matchup.
     */
    declareWinner(userId: string) {
        const winningCoach = Option.ofNullable(this.coaches.find((c) => c.id === userId));
        winningCoach.on({
            Some: (coach: Coach) => {
                this.winner = coach.teamName;
                this.done = true;
            },
            None: () => {
                throw new Error(`could not find userId ${userId} in coaches`);
            },
        });
    }

    encode(): GameData {
        const { home, away, done, winner } = this;
        return { home, away, done, winner };
    }
}
