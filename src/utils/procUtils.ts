import { execSync, spawn } from "child_process";
import { streamToString } from "./stringUtils";
import * as path from 'path';
import * as os from 'os';
import { promises as fs } from 'fs';

export type ProcessResult = {
  readonly stdout: string;
  readonly stderr: string;
  readonly exitCode: number;
};

export async function runProcess(arg0: string, args: string[]): Promise<ProcessResult> {
  const proc = spawn(arg0, args, {
    stdio: 'pipe'
  });
  const stdoutP = streamToString(proc.stdout);
  const stderrP = streamToString(proc.stderr);
  return new Promise<ProcessResult>((resolve, reject) => {
    /* eslint-disable-next-line @typescript-eslint/no-misused-promises */
    proc.once('exit', async () => {
      if ( proc.exitCode === null) {
        reject("Could not read exitCode of process");
        throw new Error("Could not read exitCode of process");
      }
      const stdout = await stdoutP;
      const stderr = await stderrP;
      const exitCode = proc.exitCode;
      resolve({stdout, stderr, exitCode});
    })
  });
}

/**
 * Interrupts current script,
 * opens file in $EDITOR,
 * waits for user to exit $EDITOR,
 * then resumes current script
 */
export function openFileInEditor(filename: string) {
    let editor = process.env['EDITOR'];
    if (!editor) {
        throw new Error(`User has no EDITOR env variable set`);
    }
    return execSync(`${editor} ${filename}`, {
        stdio: 'inherit',
        encoding: 'utf-8'
    });
}

/**
 * @param obj: 'a
 * @param toText: 'a -> string
 * @param fromText: string -> 'b
 * @returns: 'b
 */
export async function editObjectInEditor<a,b>(
    obj: a,
    toText: (x: a) => string,
    fromText: (x: string) => b
    ): Promise<b> {
    const text = toText(obj);
    const randomName = `tmp-${Math.floor(Math.random() * Math.floor(1000))}`;
    const tmpFile = path.join(os.tmpdir(), randomName);
    // create file
    // write text to file
    await fs.writeFile(tmpFile, text);
    openFileInEditor(tmpFile);
    // read file
    const result = await fs.readFile(tmpFile, {encoding: 'utf-8'});
    // translate file using `fromText`
    // return
    return fromText(result);
}