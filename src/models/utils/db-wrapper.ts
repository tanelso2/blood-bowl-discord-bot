import { Database } from 'sqlite3';

export class DBWrapper extends Database {


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