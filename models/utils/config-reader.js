const { readFileSync } = require('fs');
const logger = require('../../logger.js').child({ module: 'config-reader' });
const { Either } = require('./either.js');

const filePattern = /^\$f{(.*)}$/

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

    const fileName = matches[1];
    try {
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            return Either.Right(`Could not get fileContents of ${fileName}`);
        }
        return Either.Left(fileContents.trim());
    } catch(e) {
        return Either.Right(e.message);
    }
}

module.exports = { processConfigValue };