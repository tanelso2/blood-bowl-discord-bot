const { readFileSync } = require('fs');
const logger = require('../../logger.js').child({ module: 'config-reader' });
const { Either } = require('../../utils/types/either.js');
const { Option } = require('../../utils/types/option.js');

const filePattern = /^\$f{(.*)}$/

const defaultPattern = /^(.*):(.*)$/

/**
 * 
 * @param {String} rawValue
 * @returns {Either<String, Error>}
 */
function processConfigValue(rawValue) {
    return processFileSubstitution(rawValue).on({
       Left: (err) => Either.Right(err),
       Right: (valueOpt) => valueOpt.on({
            Some: (v) => Either.Left(v),
            _: () => Either.Left(rawValue),
        })
    });
}

// type ProcessResult =

/**
 * @returns Either<Error, Option<String>>
 */
function processFileSubstitution(value) {
    const matches = value.match(filePattern);
    if (!matches) {
        // No file substitution to do
        return Either.Right(Option.None());
    }

    const [fileName, defaultValue] = splitValueAndDefault(matches[1]);
    try {
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            throw new Error(`Could not get fileContents of ${fileName}`);
        }
        return Either.Right(Option.Some(fileContents.trim()));
    } catch(e) {
        if (defaultValue) {
            return Either.Right(Option.Some(defaultValue));
        }
        return Either.Left(e);
    }
}

function splitValueAndDefault(raw) {
    const matches = raw.match(defaultPattern);
    if (!matches) {
        return [ raw, null ];
    }
    return [ matches[1], matches[2] ];
}

module.exports = { processConfigValue };