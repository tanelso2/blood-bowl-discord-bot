import { logger } from '@core/logger';
import { Coach } from './coach';
import Discord from 'discord.js';

const LOGGER = logger.child({module: 'game'});

export interface GameData {
    home: string;
    away: string;
    done?: boolean;
}

/** One match up in a Round. */
export class Game implements GameData {
    home: string;
    away: string;
    done: boolean;
    homeCoach: Coach;
    awayCoach: Coach;

    constructor(data: GameData, coaches: Coach[]) {
        this.home = data.home;
        this.away = data.away;
        this.done = data.done || false; // false if not-exists
        this.homeCoach = coaches.find(c => c.teamNameIsCloseEnough(data.home)) || Coach.null();
        this.awayCoach = coaches.find(c => c.teamNameIsCloseEnough(data.away)) || Coach.null();
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
        const opponentOrUndef = this.coaches.find((c) => c.id !== user.id);
        if (opponentOrUndef) {
            return opponentOrUndef;
        }
        LOGGER.error(`unknown opponent of ${user.id}`);
        return Coach.null();
    }

    encode(): GameData {
        const { home, away, done } = this;
        return { home, away, done };
    }
}
