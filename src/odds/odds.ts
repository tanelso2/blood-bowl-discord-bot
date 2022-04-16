import { Either } from '@core/types/either';
const { Left, Right } = Either;

export interface OddsScenario {
    rolls: Roll[];
    modifiers: Modifier[];
}

export interface EventTree {
    event: GameEvent;
    outcomes: Outcome[];
}

export interface Outcome {
    outcome: EventTree;
    chance: number;
}

export interface RollEvent {
    kind: 'roll';
    roll: Roll;
}

export type GameEvent = RollEvent | SuccessEvent | TurnoverEvent

export type SuccessEvent = {kind:'success'}
export type TurnoverEvent = {kind:'turnover'}

export enum Modifier {
    Dodge,
    HasReroll,
    Rerolled,
    Blizzard,
    SureFeet,
    SureHands,
    Pass,
    Catch,
    Loner,
    Pro
}

export interface Roll {
    kind: RollKind;
    goal: number;
}

export enum RollKind {
    DodgeRoll,
    PickupRoll,
    ThrowRoll,
    CatchRoll,
    GFIRoll,
    ProRoll
}

const modifierParseMap: {[key: string]: Modifier} = {
    "dodge": Modifier.Dodge,
    "reroll": Modifier.HasReroll,
    "blizzard": Modifier.Blizzard,
    "surefeet": Modifier.SureFeet,
    "surehands": Modifier.SureHands,
    "pass": Modifier.Pass,
    "catch": Modifier.Catch,
    "loner": Modifier.Loner,
    "pro": Modifier.Pro
};

const rollKindParseMap: {[key: string]: RollKind} = {
    'd': RollKind.DodgeRoll,
    'p': RollKind.PickupRoll,
    'pu': RollKind.PickupRoll,
    't': RollKind.ThrowRoll,
    'c': RollKind.CatchRoll
};

const dodgeRollRegex = /([2-6])\+d/;

function parseRollOrModifier(input: string): Either<Roll, Modifier> {
    if (input in modifierParseMap) {
        return Either.Right(modifierParseMap[input]);
    } else if ("gfi" === input) {
        return Either.Left({kind: RollKind.GFIRoll, goal: 2});
    } else if (input.includes('+')) {
        const x = input.split('+');
        const goal = parseInt(x[0]);
        const kindStr = x[1];
        const kind = rollKindParseMap[kindStr];
        return Either.Left({ kind, goal });
    } else {
        throw new Error(`Cannot parse  '${input}'`);
    }
}

export function parseOddsScenario(input: string): OddsScenario {
    const rolls: Roll[] = [];
    const modifiers: Modifier[] = [];
    for(const s of input.split(/\s+/)) {
        const rOrM = parseRollOrModifier(s);
        rOrM.on({
            Left: (r) => rolls.push(r),
            Right: (m) => modifiers.push(m),
        });
    }
    return { rolls, modifiers };
}

function makeD6Outcomes(goal: number, success: EventTree, failure: EventTree): Outcome[] {
    const failureChance = (goal - 1.0) / 6.0;
    const successChance = 1.0 - failureChance;
    return [
        {outcome: success, chance: successChance},
        {outcome: failure, chance: failureChance}
    ];
}

function makeRollTree(roll: Roll, outcomes: Outcome[]): EventTree {
    return {
        event: {kind: 'roll', roll},
        outcomes
    };
}

function makeSuccessTree(): EventTree {
    return {event: {kind: 'success'}, outcomes: []};
}

function makeTurnoverTree(): EventTree {
    return {event: {kind: 'turnover'}, outcomes: []};
}

export function buildTree(scenario: OddsScenario): EventTree {
    const {rolls, modifiers} = scenario;
    if (rolls.length === 0) {
        return makeSuccessTree();
    }
    const first = rolls[0];
    const rest = rolls.slice(1);
    if (first.kind === RollKind.DodgeRoll) {
        const successTree: EventTree = buildTree({rolls: rest, modifiers});
        let failureTree: EventTree = makeTurnoverTree();
        if (Modifier.Dodge in modifiers) {
            // if dodge, replace the failure route with a route where we try the roll again.

            // if the dodge reroll succeeds, move on to the next roll after removing Dodge, since
            // a player can't use it twice in one turn
            const newSuccess = buildTree({rolls: rest, modifiers: modifiers.filter(x => x !== Modifier.Dodge)});
            // failing the reroll is just a turnover again
            const newFailure = makeTurnoverTree();
            failureTree = makeRollTree(first, makeD6Outcomes(first.goal, newSuccess, newFailure))
        } else if (Modifier.HasReroll in modifiers) {
            // if reroll
            const newSuccess = buildTree({rolls: rest, modifiers: modifiers.filter(x => x !== Modifier.HasReroll)});
            const newFailure = makeTurnoverTree();
            failureTree = makeRollTree(first, makeD6Outcomes(first.goal, newSuccess, newFailure))
        }
        const outcomes = makeD6Outcomes(first.goal, successTree, failureTree);
        return makeRollTree(first, outcomes);
    }
    return makeSuccessTree();
}

export function findSuccessProbability(tree: EventTree): number {
    const {event, outcomes} = tree;
    if (event.kind === 'success') {
        return 1.0;
    } else if (event.kind === 'turnover') {
        return 0.0;
    } else {
        let probs = 0.0;
        for (const {outcome, chance} of outcomes) {
            probs += findSuccessProbability(outcome) * chance;
        }
        return probs;
    }
}

export function parseAndFindProbability(input: string): Either<Error,number> {
    try {
        const scenario = parseOddsScenario(input);
        const eventTree = buildTree(scenario);
        const prob = findSuccessProbability(eventTree);
        return Either.Right(prob);
    } catch (e) {
        return Either.Left(e);
    }
}
