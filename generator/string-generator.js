const { directory } = require('./directory.js');

const pat = /\$\{([^ }]*)}/g;

function generateString(template) {
    const matches = template.match(pat);
    if (!matches) {
        // There is nothing to substitute
        return template;
    }

    const category = matches[1];
    const categoryList = directory[category];
    if (!categoryList) {
        return template;
    }

    const randomElement = categoryList[Math.floor(Math.random() * categoryList.length)];
    const newString = template.replace(pat, randomElement);

    // recurse
    return generateString(newString);
}

module.exports = { generateString, pat };
