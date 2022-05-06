import { Modifier, parseOddsScenario, RollKind, buildTree, findSuccessProbability } from './odds';

describe('Odds', () =>{
    describe('Parsing', () => {
        it('should parse "dodge" correctly', () => {
            const i = "dodge";
            const { rolls, modifiers } = parseOddsScenario(i);
            rolls.should.be.empty;
            modifiers.should.eql([Modifier.Dodge]);
        });

        it('should parse a dodge roll correctly', () => {
            const i = "4+d";
            const { rolls, modifiers} = parseOddsScenario(i);
            modifiers.should.be.empty;
            rolls.length.should.eql(1);
            rolls[0].should.eql({
                kind: RollKind.DodgeRoll,
                goal: 4
            });
        });

        it('should parse a dodge roll with a reroll correctly', () => {
            const i = "5+d dodge";
            const {rolls, modifiers} = parseOddsScenario(i);
            modifiers.should.eql([Modifier.Dodge]);
            rolls.length.should.eql(1);
            rolls[0].should.eql({
                kind: RollKind.DodgeRoll,
                goal: 5
            });
        });

        it('should parse terms with multiple spaces in between', () => {
            const i = "5+d    dodge";
            const {rolls, modifiers} = parseOddsScenario(i);
            modifiers.should.eql([Modifier.Dodge]);
            rolls.length.should.eql(1);
            rolls[0].should.eql({
                kind: RollKind.DodgeRoll,
                goal: 5
            });
        });

        it('should parse a pickup roll', () => {
            const i = "2+pu";
            const {rolls, modifiers} = parseOddsScenario(i);
            modifiers.length.should.eql(0);
            rolls.length.should.eql(1);
            rolls[0].should.eql({
                kind: RollKind.PickupRoll,
                goal: 2
            });
        });

        it('should parse a throw roll', () => {
            const i = "3+t";
            const {rolls, modifiers} = parseOddsScenario(i);
            modifiers.length.should.eql(0);
            rolls.length.should.eql(1);
            rolls[0].should.eql({
                kind: RollKind.ThrowRoll,
                goal: 3
            });
        });
    });
    describe('Odds', () => {
        it('one dodge roll', () => {
            const rolls = [{kind: RollKind.DodgeRoll, goal: 4}];
            const modifiers: Modifier[] = [];
            const result = buildTree({rolls, modifiers});
            result.outcomes.length.should.eql(2);
            findSuccessProbability(result).should.eql(0.5);
        });

        it('one dodge roll with dodge', () => {
            const rolls = [{kind: RollKind.DodgeRoll, goal: 4}];
            const modifiers: Modifier[] = [Modifier.Dodge];
            const result = buildTree({rolls, modifiers});
            findSuccessProbability(result).should.eql(0.75);
        });

        it('two dodges', () => {
            const rolls = [
                {kind: RollKind.DodgeRoll, goal: 4},
                {kind: RollKind.DodgeRoll, goal: 4}
            ];

            const modifiers: Modifier[] = [];
            const result = buildTree({rolls, modifiers});
            findSuccessProbability(result).should.eql(0.25);
        });

        it('one dodge roll with a reroll', () => {
            const rolls = [{kind: RollKind.DodgeRoll, goal: 4}];
            const modifiers: Modifier[] = [Modifier.HasReroll];
            const result = buildTree({rolls, modifiers});
            findSuccessProbability(result).should.eql(0.75);
        });

        it('a single dodge roll with dodge and a reroll', () => {
            const rolls = [{kind: RollKind.DodgeRoll, goal: 4}];
            const modifiers: Modifier[] = [Modifier.Dodge, Modifier.HasReroll];
            const result = buildTree({rolls, modifiers});
            findSuccessProbability(result).should.eql(0.75);
        });

        it('a 2-up dodge roll', () => {
            const rolls = [{kind: RollKind.DodgeRoll, goal: 2}];
            const modifiers: Modifier[] = [];
            const result = buildTree({rolls, modifiers});
            result.outcomes.length.should.eql(2);
            findSuccessProbability(result).should.eql(5/6);
        });
    }); 
});
