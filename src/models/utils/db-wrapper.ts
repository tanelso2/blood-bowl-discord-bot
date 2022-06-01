import { Option } from '@core/types/option';
import * as fs from 'fs';
import { Database } from 'sqlite3';

export class DBWrapper extends Database {
  constructor(filename?: string) {
    const file = filename || DEFAULT_LOCATION;
    super(file);
  }



  // SOMETHING I TRIED AND THINK WE SHOULD NOT DO:
  // async fetch<a>(sql: string, parameters: any[]): Promise<a[]> {
  // Changing these from 'any' to an abstract type 'a' might actually
  // be dangerous and make typescript's type checking less effective, leading to runtime
  // errors
  // Abstract types mean we have to declare what these will be at the call site, but in reality, the type we define 
  // and the object returned might be different, leading to runtime errors that typescript is supposed to prevent.
  // It's probably better that the callsites know, hey this could be fucking anything, you better handle it.
  async fetch(sql: string, parameters: any[]): Promise<any[]> {
    return new Promise<any[]>((resolve, reject) => {
      this.all(sql, parameters, (err, rows) => {
        if (err) {
          reject(err);
        }
        resolve(rows);
      })
    });
  }

  async fetchOne(sql: string, parameters: any[]): Promise<any> {
    const rows = await this.fetch(sql, parameters);
    /* eslint-disable-next-line @typescript-eslint/no-unsafe-return */
    return rows[0];
  }
}

const DEFAULT_LOCATION = "db/Management.db";

export class ManagementDB extends DBWrapper {
  constructor() {
    super(DEFAULT_LOCATION);
  }
}

let managementDBCache: ManagementDB | undefined = undefined;

export function getManagementDB(): Option<ManagementDB> {
  if (managementDBCache) {
    return Option.Some(managementDBCache);
  }

  if(!fs.existsSync(DEFAULT_LOCATION)) {
    return Option.None();
  }
  
  const db = new ManagementDB();
  managementDBCache = db;
  return Option.Some(db);

}
