const badTeams = [
    "Ogres",
    "Halflings",
    "Goblins",
];

const insults = [
    "Jeez, you're dumber than someone who plays ${badTeam} on purpose",
    "Nuffle utters a curse: '${curse}!'",
    "A CURSE UPON YOU: ${curse}",
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
    "Boo, choke on your ${uselessItems}!",
    "Trip over your ${uselessItems} and fall into a ${container} of ${grossBodilyFluid}",
    "You do realize this is Blood Bowl, right? Not ${babyGame}?",
    "You're so ${dumbSynonym} you put your ${clothing} on your ${bodyPart}",
    "Jeez you're more worthless than some ${container} filled with ${uselessItem}",
]
const babyGames = [
    "Fortnite",
    "Overwatch",
    "Bloons Tower Defense",
    "Hatoful Boyfriend",
    "Superman 64",
    "Goldeneye (no Odd Job, Slappers Only)",
];

const uselessItems = [
    "my hopes and dreams",
    "my resume",
    "smaller versions of itself",
    "ambition, competition, greed, envy, jealousy, hatred, injustice, treachery, and ill-health... but also hope",
    "Libra",
    "promises made by United States Senator Ted Cruz (R-TX)",
];

const dumbSynonyms = [
    "moronic",
    "goblinesque",
    "dumb",
    "smoothbrained",
    "idiotic",
    "pants-on-head stupid",
];

const clothing = [
    "sock",
    "hat",
    "pants",
    "cock sock",
    "ascot",
    "kummerbund",
    "boa",
    "graphic tee from the 90's",
    "bolo tie",
    "brass knuckles",
];

const bodyParts = [
    "head",
    "hand",
    "robotic limb... I mean... totally normal human limb",
    "...well",
    "belt loop",
    "neck",
    "ring-toe",
    "bloody stump",
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
    "storage unit",
    "Rubbermaid tub",
    "grocery bag",
    "trash can",
    "toolshed",
    "clown car",
    "dom- & sub-marine",
    "bucket",
    "assload",
    "plethora",
    "preponderance",
    "excess",
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
    "light ${colour}",
    "${colour}ish ${colour}",
];

const numbersFromOneToFive = ["1", "2", "3", "4", "5"];

export interface Directory {
    [x: string]: string[];
}

export const directory: Directory = {
    "badTeam": badTeams,
    "babyGame": babyGames,
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
