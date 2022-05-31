import { Database } from 'sqlite3';

export class DBWrapper extends Database {
  constructor(filename?: string) {
    const defaultLocation = "db/Management.db";
    const file = filename || defaultLocation;
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

export class ManagementDB extends DBWrapper {
  constructor() {
    super("db/Management.db");
  }
}
