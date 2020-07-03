const levenshtien = require('damerau-levenshtein');

function getSimilarString(s, dictionary) {
    const results = dictionary.map((word) => {
        const result = levenshtien(word, s);
        const similarity = result.similarity;
        return { word, similarity };
    });

    const minSimilarity = 0.75;
    let maxSimilarity = 0;
    let bestFit = undefined;
    for (const {word, similarity} of results) {
        if (similarity > maxSimilarity && similarity >= minSimilarity) {
            bestFit = word;
            maxSimilarity = similarity;
        }
    }
    return bestFit;
}


module.exports = { getSimilarString };
