import { readFileSync } from 'fs';
import { Either } from '@core/types/generated/either';

const filePattern = /^\$f{(.*)}$/

const defaultPattern = /^(.*):(.*)$/

/**
 * Processes config macros and returns the value.
 * So far only file substitutions exist, so this function
 * only calls that one.
 * 
 * This is intended to be the only export from the module
 * 
 * @param {String} rawValue
 * @returns {Either<String, Error>}
 */
export function processConfigValue(rawValue: string): Either<string, Error> {
    return processFileSubstitution(rawValue);
}

/**
 * Processes substitution macro that reads the contents of a file and returns that
 * 
 * @returns Either<Error, String>
 */
function processFileSubstitution(value: string): Either<string, Error> {
    const matches = filePattern.exec(value);
    if (!matches) {
        // No file substitution to do, the value is just the whole string
        return Either.Left(value);
    }

    // Case1: $f{filename}
    // Case2: $f{filename:default}
    const [fileName, defaultValue] = splitValueAndDefault(matches[1]);
    try {
        if (!fileName) {
            throw new Error(`Could not get a valid fileName from ${value}`);
        }
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            throw new Error(`Could not get fileContents of ${fileName}`);
        }
        return Either.Left(fileContents.trim());
    } catch(e) {
        if (defaultValue) {
            // Case2
            return Either.Left(defaultValue);
        }
        // Case1
        return Either.Right(e as Error);
    }
}

function splitValueAndDefault(raw: string): (string | null)[] {
    const matches = defaultPattern.exec(raw);
    if (!matches) {
        return [ raw, null ];
    }
    return [ matches[1], matches[2] ];
}
