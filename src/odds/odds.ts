import { Either } from '@core/types/generated/either';

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

export function modifierString(m: Modifier): string {
    return Modifier[m];
}

export function rollKindString(rk: RollKind): string {
    return RollKind[rk];
}

export function rollString(r: Roll): string {
    return `${r.goal}-up ${rollKindString(r.kind)}`;
}

export function scenarioString(s: OddsScenario): string {
    const rolls = s.rolls.map(x => rollString(x));
    const modifiers = s.modifiers.map(x => modifierString(x));
    return JSON.stringify({rolls, modifiers});
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
    const successTree: EventTree = buildTree({rolls: rest, modifiers});
    if (first.kind === RollKind.DodgeRoll) {
        let failureTree: EventTree = makeTurnoverTree();
        if (Modifier.Dodge in modifiers) {
            failureTree = useAutoRerollSkill(first, Modifier.Dodge, modifiers, rest);
        } else if (Modifier.HasReroll in modifiers) {
            failureTree = useTeamReroll(first, modifiers, rest);
        }
        const outcomes = makeD6Outcomes(first.goal, successTree, failureTree);
        return makeRollTree(first, outcomes);
    } else if (first.kind === RollKind.GFIRoll) {
        const goal = Modifier.Blizzard in modifiers ? 3 : 2;
        // update the roll to have the correct goal to reach since it depends on modifiers
        const updatedRoll =  {goal, kind: first.kind};
        let failureTree = makeTurnoverTree();
        if (Modifier.SureFeet in modifiers) {
            failureTree = useAutoRerollSkill(updatedRoll, Modifier.SureFeet, modifiers, rest);
        } else if (Modifier.HasReroll in modifiers) {
            failureTree = useTeamReroll(updatedRoll, modifiers, rest);
        }
        const outcomes = makeD6Outcomes(goal, successTree, failureTree);
        return makeRollTree(updatedRoll, outcomes);
    } else if (first.kind === RollKind.CatchRoll) {
        let failureTree = makeTurnoverTree();
        if (Modifier.Catch in modifiers) {
            failureTree = useAutoRerollSkill(first, Modifier.Catch, modifiers, rest);
        } else if (Modifier.HasReroll in modifiers) {
            failureTree = useTeamReroll(first, modifiers, rest);
        }
        const outcomes = makeD6Outcomes(first.goal, successTree, failureTree);
        return makeRollTree(first, outcomes);
    } else if (first.kind === RollKind.PickupRoll) {
        let failureTree = makeTurnoverTree();
        if (Modifier.SureHands in modifiers) {
            failureTree = useAutoRerollSkill(first, Modifier.SureHands, modifiers, rest);
        } else if (Modifier.HasReroll in modifiers) {
            failureTree = useTeamReroll(first, modifiers, rest);
        }
        const outcomes = makeD6Outcomes(first.goal, successTree, failureTree);
        return makeRollTree(first, outcomes);
    }
    return makeSuccessTree();
}

function useTeamReroll(roll: Roll, currentModifiers: Modifier[], rest: Roll[]): EventTree {
    const {goal} = roll;
    const success = buildTree({rolls: rest, modifiers: currentModifiers.filter(x => x !== Modifier.HasReroll)});
    const failure = makeTurnoverTree();
    return makeRollTree(roll, makeD6Outcomes(goal, success, failure));
}

function useAutoRerollSkill(roll: Roll, skill: Modifier, currentModifiers: Modifier[], rest: Roll[]): EventTree {
    const {goal} = roll;
    // On success, we can't use the auto-reroll skill again on this character this turn.
    // but we still get to move on to the rest of the rolls
    const success = buildTree({rolls: rest, modifiers: currentModifiers.filter(x => x !== skill)});
    // failing the reroll is a turnover (whether it is an injury roll or not depends on the type of thing, but let's not track that yet)
    const failure = makeTurnoverTree();
    return makeRollTree(roll, makeD6Outcomes(goal, success, failure));
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
        return Either.Left(e as Error);
    }
}
