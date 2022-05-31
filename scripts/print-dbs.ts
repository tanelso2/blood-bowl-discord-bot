import 'module-alias/register';

import { DBWrapper } from "@models/utils/db-wrapper";

const rulesDB = new DBWrapper("db/Rules.db");
const managementDB = new DBWrapper("db/Management.db");


// async function getAllTablesAndViews(db: DBWrapper): Promise<string[]> {
//     const sql = "SELECT name FROM sqlite_schema WHERE type IN ('table','view') AND name NOT LIKE 'sqlite_%'";
//     const results = await db.fetch(sql, []);
//     const names = results.map((x) => x['name']);
//     return names as string[];
// }

async function getAllTables(db: DBWrapper): Promise<string[]> {
    const sql = "SELECT name FROM sqlite_schema WHERE type='table' AND name NOT LIKE 'sqlite_%'";
    const results = await db.fetch(sql, []);
    const names = results.map((x) => x['name']);
    return names as string[];
}

async function getSchemaOfTable(db: DBWrapper, name: string): Promise<string> {
    const sql = "SELECT sql FROM sqlite_master WHERE type='table' AND name=?";
    const result = await db.fetchOne(sql, [name]);
    return result['sql'] as string;
}

async function countRowsOfTable(db: DBWrapper, name: string): Promise<number> {
    // Hello sql injection? Yes please
    const sql = `SELECT COUNT(*) as count FROM ${name}`;
    const result = await db.fetchOne(sql, []);
    return parseInt(result['count']);
}

const divider = `~~~~~~~~~~~~~~~~~~~~~~~~~~`;

const interestingTables: string[] = [

];

async function showTableInformation(db: DBWrapper, name: string) {
    const schema = await getSchemaOfTable(db, name);
    const rowCount = await countRowsOfTable(db, name);
    if (rowCount > 0) {
        console.log(divider);
        console.log(` TABLE: ${name} `);
        console.log(`    ROWS: ${rowCount}`);
        prettyPrintSchema(schema);
        if (name in interestingTables) {
            await showSeveralRows(db, name);
        } else {
            await showExampleRow(db, name);
        }
    }
}

async function showExampleRow(db: DBWrapper, name: string) {
    const sql = `SELECT * FROM ${name} LIMIT 1`;
    const result = await db.fetchOne(sql, []);
    console.log(`    EXAMPLE: ${JSON.stringify(result)}`);
}


async function showSeveralRows(db: DBWrapper, name: string) {
    const limit = 10;
    const sql = `SELECT * FROM ${name} LIMIT ${limit}`;
    const results = await db.fetch(sql, []);
    for (const result of results) {
        console.log(`    EXAMPLE: ${JSON.stringify(result)}`);
    }

}



const creationPat = /CREATE TABLE (\w+)\((.+)\)/;

function prettyPrintSchema(schema: string) {
    const matches = creationPat.exec(schema);
    if (!matches) {
        throw new Error("Well shitbags");
    }

    const name = matches[1];
    const columns = matches[2].split(`,`).map((x) => x.trim());
    console.log(`    CREATE TABLE ${name}`);
    for (const col of columns) {
        console.log(`       ${col}`);
    }
}

async function main() {
    console.log(`Rules`);
    const tables = await getAllTables(rulesDB);
    for(const table of tables) {
        await showTableInformation(rulesDB, table);
    }
    console.log(`\n\n\n`);

    console.log(`Management`);
    const mtables = await getAllTables(managementDB);
    for(const table of mtables) {
        await showTableInformation(managementDB, table);
    }
}


main();