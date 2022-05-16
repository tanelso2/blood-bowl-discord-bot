import { DBWrapper } from './utils/db-wrapper';
import { PlayerType } from './playertype';

describe('Database stuff', () => {
  const db = new DBWrapper("/Users/tnelson/Library/Application Support/BloodBowl2/DB/Rules.db");
  it('Should create properly', async () => {
    const playerType = PlayerType.fetchFromDb(db, 701);
    console.log(`PlayerType returned as ${JSON.stringify(playerType)}`);
  });

});