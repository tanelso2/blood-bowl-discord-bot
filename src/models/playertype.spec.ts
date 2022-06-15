import { DBWrapper, getManagementDB, ManagementDB } from './utils/db-wrapper';
import { TeamType } from './teamtype';


let db: DBWrapper;

// Selectively enable tests if the database files are available
const des = getManagementDB().on({
  Some: (x: ManagementDB) => {
    db = x;
    return describe;
  },
  None: () => {
    console.debug(`Skipping database tests`);
    return describe.skip;
  }
});

des('Database stuff', () => {
  describe('Team types', () => {
    it('should produce output small enough for all teams', async () => {
      const teams = await TeamType.getAllTeamTypes(db);
      for (const team of teams) {
        const playersReply = await team.getPlayersReferenceString();
        const starPlayersReply = await team.getStarPlayersReferenceString();
        playersReply.length.should.be.lessThan(1000);
        starPlayersReply.length.should.be.lessThan(1000);
      }
    });
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
