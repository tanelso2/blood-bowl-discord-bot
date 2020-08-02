const { readFileSync } = require('fs');
const logger = require('../../logger.js').child({ module: 'config-reader' });
const { Either } = require('../../utils/types/either.js');

const filePattern = /^\$f{(.*)}$/

const defaultPattern = /^(.*):(.*)$/

/**
 * 
 * @param {String} value 
 * @returns {Either<String, Error>}
 */
function processConfigValue(value) {
    const matches = value.match(filePattern);
    if (!matches) {
        // No file substitution to do
        return Either.Left(value);
    }

    const [fileName, defaultValue] = splitValueAndDefault(matches[1]);
    try {
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            throw new Error(`Could not get fileContents of ${fileName}`);
        }
        return Either.Left(fileContents.trim());
    } catch(e) {
        if (defaultValue) {
            return Either.Left(defaultValue);
        }
        return Either.Right(e.message);
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