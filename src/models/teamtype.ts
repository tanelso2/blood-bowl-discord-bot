import { DBWrapper } from './utils/db-wrapper';
import { PlayerType } from './playertype';

export class TeamType {
    public id: string;
    public name: string;
    private db: DBWrapper;

    constructor(id: string, name: string, db: DBWrapper) {
        this.name = name;
        this.id = id;
        this.db = db;
    }

    async getStarPlayers(): Promise<PlayerType[]> {
        const sql = `SELECT
                        IdPlayerTypes as id 
                     FROM bb_rules_races_star_players
                     WHERE IdRaces = ?`;
        const results = await this.db.fetch(sql, [this.id]);
        const ids = results.map(x => parseInt(x["id"]));
        return Promise.all(
            ids.map(x => PlayerType.getFromId(this.db, x))
        );
    }

    async getPlayerTypes(): Promise<PlayerType[]> {
        const sql = `SELECT
                       ID
                     FROM bb_rules_player_types
                     WHERE IdRaces = ?`;
        const results = await this.db.fetch(sql, [this.id]);
        const ids = results.map(x => parseInt(x["ID"]));
        return Promise.all(
            ids.map(x => PlayerType.getFromId(this.db, x))
        );
    }

    static async getTeamTypeFromName(db: DBWrapper, name: string): Promise<TeamType> {
        const sql = `SELECT ID, DataConstant
                     FROM bb_rules_races
                     WHERE DataConstant LIKE ?`;
        const results = await db.fetchOne(sql, [name]);
        if(!results) {
            throw new Error(`Couldn't find a team type called ${name}`);
        }
        return new TeamType(results["ID"], results["DataConstant"], db);
    }
}