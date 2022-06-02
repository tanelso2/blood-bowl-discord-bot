import { Option } from '@core/types/option';
import * as fs from 'fs';
import { Database } from 'sqlite3';

function allVerify<a>(l: unknown[], verify: (x: unknown) => x is a): l is a[] {
  return l.every(x => verify(x));
}

export class DBWrapper extends Database {
  constructor(filename?: string) {
    const file = filename || DEFAULT_LOCATION;
    super(file);
  }

  async fetch<a>(sql: string, parameters: any[]): Promise<Partial<a>[]> {
    return new Promise<Partial<a>[]>((resolve, reject) => {
      this.all(sql, parameters, (err, rows) => {
        if (err) {
          reject(err);
        }
        resolve(rows as Partial<a>[]);
      })
    });
  }

  async fetchOne<a>(sql: string, parameters: any[]): Promise<Partial<a>> {
    const rows = await this.fetch(sql, parameters);
    /* eslint-disable-next-line @typescript-eslint/no-unsafe-return */
    return rows[0] as Partial<a>;
  }

  async fetchAndVerify<a>(sql: string, parameters: any[], verify: (x: unknown) => x is a): Promise<a[]> {
    const results = await this.fetch<a>(sql, parameters);
    if(!allVerify(results, verify)) {
      throw new Error("fuck");
    }
    return results;
  }

  async fetchAndVerifyOne<a>(sql: string, parameters: any[], verify: (x: unknown) => x is a): Promise<a> {
    const r = await this.fetchOne<a>(sql, parameters);
    if(!verify(r)) {
      throw new Error("fuck");
    }
    return r;
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
