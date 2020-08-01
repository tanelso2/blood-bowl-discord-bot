const { readFileSync } = require('fs');
const logger = require('../../logger.js').child({ module: 'config-reader' });

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
        return value;
    }

    const fileName = matches[1];
    try {
        const fileContents = readFileSync(fileName, {encoding: "utf-8"});
        if (!fileContents) {
            return new Exception(`Could not get fileContents of ${fileName}`);
        }
        return fileContents.trim();
    } catch(e) {
        return new Exception(e.message);
    }
}

module.exports = { processConfigValue };