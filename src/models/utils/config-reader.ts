import { readFileSync } from 'fs';
import { Either } from '@core/types/generated/either';
import { Option } from '@core/types/generated/option';

const filePattern = /^\$f{(.*)}$/

const defaultPattern = /^(.*):(.*)$/

/**
 *
 * @param {String} rawValue
 * @returns {Either<String, Error>}
 */
export function processConfigValue(rawValue: string): Either<string, Error> {
    return processFileSubstitution(rawValue).on({
       Left: (err: Error) => Either.Right(err),
       Right: (valueOpt: Option<string>) => valueOpt.on({
            Some: (v: string) => Either.Left(v),
            _: () => Either.Left(rawValue),
        })
    });
}

// type ProcessResult =

/**
 * @returns Either<Error, Option<String>>
 */
function processFileSubstitution(value: string): Either<Error, Option<String>> {
    const matches = filePattern.exec(value);
    if (!matches) {
        // No file substitution to do
        return Either.Right(Option.None());
    }

    const [fileName, defaultValue] = splitValueAndDefault(matches[1]);
    try {
        if (!fileName) {
            throw new Error(`Could not get a valid fileName from ${value}`);
        }
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            throw new Error(`Could not get fileContents of ${fileName}`);
        }
        return Either.Right(Option.Some(fileContents.trim()));
    } catch(e) {
        if (defaultValue) {
            return Either.Right(Option.Some(defaultValue));
        }
        return Either.Left(e as Error);
    }
}

function splitValueAndDefault(raw: string): (string | null)[] {
    const matches = defaultPattern.exec(raw);
    if (!matches) {
        return [ raw, null ];
    }
    return [ matches[1], matches[2] ];
}
