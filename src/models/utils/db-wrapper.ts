import { Option } from '@core/types/option';
import * as fs from 'fs';
import { Database } from 'sqlite3';

export class DBWrapper extends Database {
  constructor(filename?: string) {
    const file = filename || DEFAULT_LOCATION;
    super(file);
  }

  async fetch(sql: string, parameters: any[]): Promise<any[]> {
    return new Promise<any>((resolve, reject) => {
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
