declare module "damerau-levenshtien";

export interface DamerauLevenshtienResponse {
    steps: number;
    relative: number;
    similarity: number;
}

export default function levenshtien(x: string, y: string, limit?: number): DamerauLevenshtienResponse;

