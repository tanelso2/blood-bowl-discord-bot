import 'module-alias/register';

import { patternLanguage } from '@core/types/patterns/lang';
import { patternDefToTS } from '@core/types/patterns/codeGen';
// import * as process from 'process';

import * as fs from 'fs/promises';
import path from 'path';
import { runProcess } from '@utils/procUtils';


const srcDir = path.normalize('./src');

async function exists(path: string): Promise<boolean> {
    try {
        await fs.access(path);
        return true;
    } catch {
        return false;
    }
}

async function handlePatFile(fPath: string) {
    const { dir, name } = path.parse(fPath);
    const generatedDir = path.join(dir, "generated");
    if (!await exists(generatedDir)) {
        console.log(`Creating ${generatedDir}`);
        await fs.mkdir(generatedDir);
    }
    const patInputBuffer = await fs.readFile(fPath);
    const patInput = patInputBuffer.toString('utf-8');
    const patDef = patternLanguage.patternDef.tryParse(patInput);
    const typescriptOutput = patternDefToTS(patDef);
    const outputFile = path.join(generatedDir, `${name}`);
    console.log(`Gonna write to ${outputFile}`);
    await fs.writeFile(outputFile, typescriptOutput);
    console.log(`Gonna tsfmt ${outputFile}`);
    const result = await runProcess('tsfmt', ['-r', outputFile]);
    if (result.exitCode !== 0) {
        console.error(`ERROR - tsfmt failed`);
        throw new Error(`tsfmt failed`);
    }
}

async function handleDir(d: string) {
    const dir = await fs.opendir(d);
    for await (const x of dir) {
        const xPath = path.join(d, x.name);
        if (x.isFile() && x.name.endsWith('.ts.patn')) {
            console.log(`Found: ${xPath}`);
            await handlePatFile(xPath);
        } else if (x.isDirectory()) {
            await handleDir(xPath);
        }
    }

}

async function main() {
    await handleDir(srcDir);
}

void main();
