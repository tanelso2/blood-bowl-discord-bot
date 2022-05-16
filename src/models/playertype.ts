import { DBWrapper } from './utils/db-wrapper';

export class PlayerType {

  constructor(
    public name: string,
    public skillNames: string[],
    public movementAllowance: number,
    public strength: number,
    public armorValue: number,
    public agility: number,
    public cost: number,
    public normalSkillAccess: string[],
    public doublesSkillAccess: string[]
  ) {
  }

  static async fetchFromDb(db: DBWrapper, id: number): Promise<PlayerType> {
    let name = "";
    let skillNames: string[] = [];
    let movementAllowance = 0;
    let strength = 0;
    let armorValue = 0;
    let agility = 0;
    let cost = 0;
    let normalSkillAccess: string[] = [];
    let doublesSkillAccess: string[] = [];

    const results = await db.fetchOne("SELECT * FROM PlayerTypes WHERE ID=?", [id]);
    agility = results["CharacsAgility"];
    armorValue = results["CharacsArmourValue"];
    cost = results["Price"];
    strength = results["CharacsStrength"];
    movementAllowance = results["CharacsMovementAllowance"];

    // TODO: Get the name somehow? Localizations might make this difficult....
    // TODO: Get the names of the skills
    // TODO: Get the levelup skills categories

    const player = new PlayerType(name, skillNames, movementAllowance, strength, armorValue, agility, cost, normalSkillAccess, doublesSkillAccess);
    console.log(`Player is ${JSON.stringify(player)}`);
    return player;
  }
}

