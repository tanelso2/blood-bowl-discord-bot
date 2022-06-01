import { DBWrapper, getManagementDB, ManagementDB } from './utils/db-wrapper';
import { TeamType } from './teamtype';

// TODO: Selectively enable tests if the database files are available

let db: DBWrapper;

let des = getManagementDB().on({
  Some: (x) => {
    db = x;
    return describe;
  },
  None: () => {
    console.debug(`Skipping database tests`);
    return describe.skip;
  }
});

des('Database stuff', () => {
  it('all teams', async () => {
    const sql = `SELECT
                   DataConstant as name
                 FROM bb_rules_races
                 WHERE IsTeam = 1`;
    const results = await db.fetch(sql, []);
    const names = results.map(x => x["name"]);
    console.debug(names);
  });

  describe('Team types', () => {
    it('should find Human', async () => {
      const name = 'Human';
      const teamType = await TeamType.getTeamTypeFromName(db, name);
      const playerTypes = await teamType.getPlayerTypes();
      playerTypes.length.should.not.eql(0);
      const starPlayers = await teamType.getStarPlayers();
      starPlayers.length.should.not.eql(0);
      const griff = starPlayers.find(x => x.name.includes("Griff"));
      if (!griff) {
        throw new Error("Griff unfound. My heart yearns");
      }
      griff.movementAllowance.should.eql(7);
    });

    it('should find human', async () => {
      const name = 'human';
      const teamType = await TeamType.getTeamTypeFromName(db, name);
      const playerTypes = await teamType.getPlayerTypes();
      playerTypes.length.should.not.eql(0);
      const starPlayers = await teamType.getStarPlayers();
      starPlayers.length.should.not.eql(0);
      const griff = starPlayers.find(x => x.name.includes("Griff"));
      if (!griff) {
        throw new Error("Griff unfound. My heart yearns");
      }
      griff.movementAllowance.should.eql(7);
      console.log(griff.toPrettyString());
    });

    it('should find underworld', async () => {
      const name = 'Underworld';
      const teamType = await TeamType.getTeamTypeFromName(db, name);
      const playerTypes = await teamType.getPlayerTypes();
      playerTypes.length.should.not.eql(0);
      const starPlayers = await teamType.getStarPlayers();
      starPlayers.length.should.not.eql(0);
    });
  });
});
