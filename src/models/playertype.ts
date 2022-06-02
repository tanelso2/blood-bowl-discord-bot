import { DBWrapper } from './utils/db-wrapper';

type PlayerTypeRow = {
  DataConstant: string;
  CharacsAgility: number;
  CharacsArmourValue: number;
  Price: number;
  CharacsStrength: number;
  CharacsMovementAllowance: number;
};

function verifyPlayerTypeRow(x: any): x is PlayerTypeRow {
  return "DataConstant" in x
      && "CharacsAgility" in x
      && "CharacsArmourValue" in x
      && "Price" in x
      && "CharacsStrength" in x
      && "CharacsMovementAllowance" in x;
}

export class PlayerType {
  name: string;
  skillNames: string[];
  movementAllowance: number;
  strength: number;
  armorValue: number;
  agility: number;
  cost: number;
  normalSkillAccess: string[];
  doublesSkillAccess: string[];
  private id: string;

  private constructor(
    id: string,
    name: string,
    movementAllowance: number,
    strength: number,
    armorValue: number,
    agility: number,
    cost: number,
    skillNames: string[],
    normalSkillAccess: string[],
    doublesSkillAcess: string[]
  ) {
    this.id = id;
    this.name = name;
    this.movementAllowance = movementAllowance;
    this.strength = strength;
    this.armorValue = armorValue;
    this.agility = agility;
    this.cost = cost;
    this.skillNames = skillNames;
    this.normalSkillAccess = normalSkillAccess;
    this.doublesSkillAccess = doublesSkillAcess;
  }


  static async getFromId(db: DBWrapper, id: number): Promise<PlayerType> {
    const sql = `SELECT * FROM bb_rules_player_types WHERE ID = ?`;
    const result = await db.fetchAndVerifyOne<PlayerTypeRow>(sql, [id], verifyPlayerTypeRow);
    const name = result["DataConstant"];
    const agility = result["CharacsAgility"];
    const armorValue = result["CharacsArmourValue"];
    const cost = result["Price"];
    const strength = result["CharacsStrength"];
    const movementAllowance = result["CharacsMovementAllowance"];
    const skillNames = await getSkillNames(db, id);
    const normalSkillAccess = await getNormalSkillAccess(db, id);
    const doublesSkillAccess = await getDoublesSkillAccess(db, id);
    return new PlayerType(
      id.toString(), 
      name, 
      movementAllowance, 
      strength, 
      armorValue, 
      agility, 
      cost, 
      skillNames, 
      normalSkillAccess, 
      doublesSkillAccess
    );
  }

  toString(): string {
    return `PlayerType ${this.id} - ${this.name}`;
  }

  toPrettyString(): string {
    const s = `${this.name} 
      ST: ${this.strength}    AG: ${this.agility}
      AV: ${this.armorValue}    MA: ${this.movementAllowance}
      Skills: ${this.skillNames.join(", ")}
      Cost: ${this.cost}
      Level ups
      Normal: ${this.normalSkillAccess.join(", ")}
      Doubles: ${this.doublesSkillAccess.join(", ")}`;
    return s.split('\n')
            .map(x => x.trim())
            .join('\n');
  }

}

type NAME = {name: string};

function isName(x: any): x is NAME {
  return "name" in x;
}

async function getSkillNames(db: DBWrapper, playerTypeId: number): Promise<string[]> {
  const sql = `SELECT  
                bb_rules_skill_listing.DataConstant as name
               FROM
               bb_rules_skill_listing 
                JOIN bb_rules_player_type_skills
                 ON bb_rules_skill_listing.ID = bb_rules_player_type_skills.IdSkillListing
               WHERE bb_rules_player_type_skills.IDPlayerTypes = ?`;
  const results = await db.fetchAndVerify<NAME>(sql, [playerTypeId], isName);
  const names = results.map(x => x.name);
  return names;
}

async function getNormalSkillAccess(db: DBWrapper, playerTypeId: number): Promise<string[]> {
  const sql = `SELECT
                bb_rules_skill_categories.DataConstant as name
               FROM
               bb_rules_player_type_skill_categories_normal
                 JOIN bb_rules_skill_categories
                  ON bb_rules_player_type_skill_categories_normal.IdSkillCategories = bb_rules_skill_categories.ID
                WHERE bb_rules_player_type_skill_categories_normal.IDPlayerTypes = ?`;
  const results = await db.fetch<NAME>(sql, [playerTypeId]);
  const names = results.map(x => x.name!);
  return names;
}

async function getDoublesSkillAccess(db: DBWrapper, playerTypeId: number): Promise<string[]> {
  const sql = `SELECT
                bb_rules_skill_categories.DataConstant as name
               FROM
               bb_rules_player_type_skill_categories_double
                 JOIN bb_rules_skill_categories
                  ON bb_rules_player_type_skill_categories_double.IdSkillCategories = bb_rules_skill_categories.ID
                WHERE bb_rules_player_type_skill_categories_double.IDPlayerTypes = ?`;
  const results = await db.fetch<NAME>(sql, [playerTypeId]);
  const names = results.map(x => x.name!);
  return names;
}