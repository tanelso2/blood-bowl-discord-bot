const badTeams = [
    "Ogres",
    "Haflings",
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

const numbersFromOneToFive = [...Array(5).keys()].map(x => (x + 1).toString());

const directory = {
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

module.exports = { directory };
