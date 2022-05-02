declare module "damerau-levenshtein";

export interface DamerauLevenshteinResponse {
    steps: number;
    relative: number;
    similarity: number;
}

export default function levenshtein(x: string, y: string, limit?: number): DamerauLevenshteinResponse;

