import { Stream } from 'stream';

import levenshtein from 'damerau-levenshtein';

export function getSimilarity(x: string, y: string): number {
    const result = levenshtein(x, y);
    return result.similarity;
}

export function getSimilarString(s: string, dictionary: string[]): string | undefined {
    const results = dictionary.map((word) => {
        const result = levenshtein(word, s);
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

export async function streamToString (stream: Stream): Promise<string> {
    const chunks: Buffer[] = [];
    return new Promise((resolve, reject) => {
        stream.on('data', (chunk) => chunks.push(Buffer.from(chunk)));
        stream.on('error', (err) => reject(err));
        stream.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')));
    });
}

export function trimMultilineLiteral(s: string): string {
    return s.split('\n')
            .map(x => x.trim())
            .join('\n');
}