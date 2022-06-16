import 'module-alias/register';

import * as process from 'process';
import { ProcessResult, runProcess } from '@utils/procUtils';

async function runNpmTask(task: string): Promise<ProcessResult> {
  console.debug(`Starting task ${task}`);
  return runProcess('npm', ['run', task]);
}

async function main() {
  const tasks = ['compile', 'test', 'lint'];
  const results = await Promise.all(tasks.map(x => runNpmTask(x)));
  for (const r of results) {
    console.log(r.stdout);
  }
  const exitCode = results.map(x => x.exitCode)
                          .reduce((acc, x) => acc | x);
  process.exit(exitCode);
}

void main();