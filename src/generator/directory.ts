/**
 * Want to contribute to the insult generator? This is the file you should do it on!
 *
 * How it works:
 * Whenever the generator encounters a substring that looks like ${category}, it looks up that category in the directory and replaces it with a random string from that category.
 * These strings can also contain other category references, so the generator will continue until there are no more ${category} substrings.
 * The directory is at the bottom of this file. When editing the file, try to keep it there.
 *
 * How do I add to it?
 * Just come up with more categories and/or add to existing categories.
 * If you do add another category, make sure to add it to the directory.
 *
 * What if I want to add but don't actually know this programming language?
 * Just give it your best shot. We'll make sure your syntax is correct before merging your changes in, no big deal.
 */


const badTeams = [
    "Ogres",
    "Halflings",
    "Goblins",
];

const insults = [
    "Jeez, you're dumber than someone who plays ${badTeam} on purpose",
    "Nuffle utters a curse: '${curse}!'",
    "A CURSE UPON YOU: ${curse}",
    "You're so ${dumbSynonym} you put your ${clothing} on your ${bodyPart}",
    "Jeez you're more worthless than some ${container} filled with ${uselessItem}",
]

const uselessItems = [
    "my hopes and dreams",
];

const dumbSynonyms = [
    "moronic",
    "dumb",
    "smoothbrained",
    "idiotic",
    "pants-on-head stupid",
];

const clothing = [
    "sock",
    "hat",
    "pants",
];

const bodyParts = [
    "head",
    "hand",
    "robotic limb... I mean... totally normal human limb",
];

const curses = [
    "May you only be left with ${numberFrom1To5} players standing at the end of your next game",
    "May all your GFIs fail!",
    "May all your block dice come up ${notGoodBlockDice}",
];

const notGoodBlockDice = [
    "Push",
    "Both Down when your opponent has Block and you don't",
    "Blue Pow when you're playing dodgy teams",
    "Skulls",
    "Skulls, just nothing but Skulls",
    "a literal infinite ${container} of Skulls"
];


const containers = [
    "ocean",
    "pool",
    "box",
    "toolshed",
    "clown car",
    "dom- & sub-marine",
    "bucket",
    "fuckbucket",
    "fuck${container}",
    "urinal covered in ${grossBodilyFluid}"
];

const grossBodilyFluids = [
    "blood",
    "shit",
    "piss",
    "${grossBodilyFluid} & ${grossBodilyFluid}",
    "smegma",
    "somebody's snot",
    "blood that's slightly ${color}",
    "earwax",
    "candle-nope nope I was wrong, that's definitely just ear-wax"
];

const colors = [
    "red",
    "green",
    "grey",
    "pink",
    "brown",
    "blue",
    "greyish ${colour}",
    "brownish ${colour}",
    "dark ${color}",
    "light ${colour}"
];

const numbersFromOneToFive = ["1", "2", "3", "4", "5"];

export interface Directory {
    [x: string]: string[];
}

export const directory: Directory = {
    "badTeam": badTeams,
    "bodyPart": bodyParts,
    "clothing": clothing,
    "color": colors,
    "colour": colors,
    "container": containers,
    "curse": curses,
    "dumbSynonym": dumbSynonyms,
    "grossBodilyFluid": grossBodilyFluids,
    "insult": insults,
    "notGoodBlockDice": notGoodBlockDice,
    "numberFrom1To5": numbersFromOneToFive,
    "uselessItem": uselessItems,
};
