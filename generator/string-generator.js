const { directory } = require('./directory.js');
const logger = require('../logger.js').child({ module: 'string-generator'});

const singularPat = /\$\{([^ }]*)}/;
const pat = new RegExp(singularPat, 'g');


function generateString(template) {
    const matches = [...template.matchAll(pat)].map(x => x[1]);
    if (matches.length === 0) {
        // There is nothing to substitute
        //logger.debug('No matches');
        return template;
    }
    //logger.debug(matches);

    // Just grab the first one, the other matches will be 
    // taken care of in the recursion
    const category = matches[0];
    const categoryList = directory[category];
    if (!categoryList) {
        return template;
    }

    const randomElement = categoryList[Math.floor(Math.random() * categoryList.length)];
    const newString = template.replace(singularPat, randomElement);
    //logger.debug(`template = ${template}, newString = ${newString}`)

    // recurse
    return generateString(newString);
}

module.exports = { generateString, pat };
