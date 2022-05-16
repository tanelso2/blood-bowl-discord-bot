import { DBWrapper } from './db-wrapper';

describe('DBWrapper', () => {
  it('testing', async () => {
    const db = new DBWrapper("/Users/tnelson/Library/Application Support/BloodBowl2/DB/Rules.db");
    const x = await db.fetchOne("SELECT * FROM PlayerTypes WHERE ID = ?", [701])
    console.log(`x is ${JSON.stringify(x)}`);

  });

});