import { DBWrapper } from './utils/db-wrapper';
import { PlayerType } from './playertype';

type ID = {ID: string};

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
                        bb_rules_player_types.ID
                     FROM bb_rules_races_star_players
                       JOIN bb_rules_player_types
                         ON bb_rules_player_types.ID=bb_rules_races_star_players.IdPlayerTypes
                     WHERE bb_rules_races_star_players.IdRaces = ?
                     AND bb_rules_player_types.DataConstant NOT LIKE '%_Fallback'`;
        const results = await this.db.fetch(sql, [this.id]) as ID[];
        const ids = results.map(x => parseInt(x.ID));
        return Promise.all(
            ids.map(x => PlayerType.getFromId(this.db, x))
        );
    }

    async getPlayerTypes(): Promise<PlayerType[]> {
        const sql = `SELECT
                       ID 
                     FROM bb_rules_player_types
                     WHERE IdRaces = ?`;
        const results = await this.db.fetch(sql, [this.id]) as ID[];
        const ids = results.map(x => parseInt(x.ID));
        return Promise.all(
            ids.map(x => PlayerType.getFromId(this.db, x))
        );
    }

    static async getTeamTypeFromName(db: DBWrapper, name: string): Promise<TeamType> {
        const sql = `SELECT ID, DataConstant
                     FROM bb_rules_races
                     WHERE DataConstant LIKE ?`;
        type result = ID & {DataConstant: string};
        const results = await db.fetchOne<result>(sql, [name]);
        if(!results) {
            throw new Error(`Couldn't find a team type called ${name}`);
        }
        return new TeamType(results["ID"]!, results["DataConstant"]!, db);
    }

    static async getAllTeamTypes(db: DBWrapper): Promise<TeamType[]> {
        const sql = `SELECT ID, DataConstant
                     FROM bb_rules_races`;
        type result = ID & {DataConstant: string};
        function isResult(x: any): x is result {
            return "ID" in x && "DataConstant" in x;
        }
        const r = await db.fetchResults<result>(sql, [], isResult);
        return r.on({
            Result: (r: result[]) => {
                const banlist = [`Extra`, `Common`, `StarPlayer`, `AllStars`];
                return r
                    .filter(x => {
                        const name = x.DataConstant;
                        const bannedName = banlist.includes(name) || name.includes("Mercenary");
                        return !bannedName;
                    })
                    .map(x => new TeamType(x.ID, x.DataConstant, db));
            },
            _: () => {
                throw new Error("Failed to get results");
            }
        });
    }

    async getPlayersReferenceString(): Promise<string> {
        const players = await this.getPlayerTypes();
        const playerStrs = players.map(x => x.toPrettyString());
        return codeblock(playerStrs.join(`\n\n`));
    }

    async getStarPlayersReferenceString(): Promise<string> {
        const starPlayers = await this.getStarPlayers();
        const playerStrs = starPlayers.map(x => x.toPrettyStringWithoutLevelUps());
        return codeblock(playerStrs.join(`\n\n`));
    }
}

function codeblock(s: string): string {
    return '```\n' + s + '\n```';
}