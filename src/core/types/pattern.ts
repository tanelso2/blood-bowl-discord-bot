/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-unsafe-return */
import { ErrorList } from '@utils/errorList';
import * as Pars from 'parsimmon';

export function parseCharsUntil(x: Pars.Parser<any>): Pars.Parser<string> {
    const p = Pars.lookahead(x).or(Pars.any).assert(x => x !== '', 'found the lookahead');
    return p.many().tie();
}

export function parseCharsBefore(s: string): Pars.Parser<string> {
    const p = Pars.Parser((input, i) => {
        const next = input.slice(i, i + s.length);
        if (next === s) {
            return Pars.makeFailure(i, "");
        }
        return Pars.makeSuccess(i+1, input.charAt(i))
    });
    return p.many().tie();
}

function anythingSurroundedBy(lbound: Pars.Parser<any>, rbound: Pars.Parser<any>): Pars.Parser<string> {
    return parseCharsUntil(rbound).wrap(lbound, rbound);
}


export interface PatternLanguage {
    notnewline: string;
    pat: string;
    extraDefsOpen: string;
    extraDefsClose: string;
    extraDefs: string;
    identifier: string;
    ctor: PatternConstructor;
    ctorSep: string;
    ctorList: PatternConstructor[];
    openBracket: string;
    closeBracket: string;
    genericList: string[];
    generics: string[];
    patternDef: PatternDefinition;
    eqls: string;
    parenOpen: string;
    parenClose: string;
    lines: string[];
}
export const patternLanguage = Pars.createLanguage<PatternLanguage>({
    notnewline: () => Pars.regexp(/[^\n]*/),
    eqls: () => Pars.string("=").trim(Pars.whitespace),
    pat: () => Pars.string("pat").trim(Pars.optWhitespace),
    extraDefsOpen: () => Pars.string("{|"),
    extraDefsClose: () => Pars.string("|}").trim(Pars.optWhitespace),
    extraDefs: (r) => anythingSurroundedBy(r.extraDefsOpen, r.extraDefsClose).or(Pars.of('')),
    lines: (r) => r.notnewline.sepBy(Pars.string("\n")),
    identifier: () => Pars.regexp(/[A-Za-z]+/),
    ctor: (r) => {
        return r.identifier.sepBy1(Pars.whitespace)
            .map(ids => {
                return {
                    name: ids[0],
                    args: ids.slice(1)
                } as PatternConstructor;
            });
    },
    ctorSep: () => Pars.string("|").trim(Pars.optWhitespace),
    ctorList: (r) => r.ctor.sepBy1(r.ctorSep).wrap(r.parenOpen, r.parenClose),
    parenOpen: () => Pars.string("(").trim(Pars.optWhitespace),
    parenClose: () => Pars.string(")").trim(Pars.optWhitespace),
    openBracket: () => Pars.string("<"),
    closeBracket: () => Pars.string(">"),
    genericList: (r) => r.identifier.sepBy1(Pars.string(",").trim(Pars.optWhitespace)),
    generics: (r) => r.genericList.wrap(r.openBracket, r.closeBracket).or(Pars.of([])),
    patternDef: (r) => {
        return Pars.seqMap(r.pat, r.identifier, r.generics, r.ctorList, r.extraDefs, 
            (_, name, generics, ctors, extraDefs) => {
                return {
                    name,
                    generics,
                    extraDefs,
                    constructors: ctors
                };
        });
    }
});

export const WILDCARD = "_";

export interface WildcardPattern<a> {
    '_': () => a
}

export interface PatternConstructor {
    name: string;
    args: string[];
}

export interface PatternDefinition {
    name: string;
    generics: string[];
    constructors: PatternConstructor[];
    extraDefs: string;
}

function makeGenericsTemplate(generics: string[]): string {
    if (generics.length === 0) {
        return '';
    } else {
        const g = generics.join(',');
        return `<${g}>`; 
    }
}

function makeFuncTypeSig(c: PatternConstructor, retType: string): string {
    return `(${constructorArgs(c)}) => ${retType}`;
}

interface VariableDef {
    name: string;
    type: string;
}

function makeVariableDefs(c: PatternConstructor): VariableDef[] {
    return c.args.map((argType, index) => {
        return {
            name: `arg${index}`,
            type: argType
        }
    });
}

function argNames(c: PatternConstructor): string[] {
    return makeVariableDefs(c).map(x => x.name);
}

function constructorArgs(c: PatternConstructor): string {
    return makeVariableDefs(c)
        .map(({name, type}) => `${name}: ${type}`)
        .join(', ');
}

function baseClassConstructor(p: PatternDefinition): string {
    const constructorNames = p.constructors.map(x => x.name);
    const constructorString = `
        constructor() {
            super([${constructorNames.join(', ')}]);
        }
    `;
    return constructorString;
}

export function patternDefToTS(p: PatternDefinition): string {
    const fullPatternInterfaceDef = makeFullPatternInterfaceDef(p);
    const patternTypeDef = makePatternTypeDef(p);
    const baseClassDef = baseClass(p);
    const constructorClassDefs = p.constructors.map(x => constructorClassDef(p, x)).join('\n\n');
    return `
        /* eslint-disable @typescript-eslint/no-explicit-any */
        /* eslint-disable @typescript-eslint/no-unused-vars */
        /* eslint-disable @typescript-eslint/no-unsafe-return */
        import { PatternMatchable, Pattern, StringsToFunctions } from '@core/types/pattern';

        ${fullPatternInterfaceDef}
         
        ${patternTypeDef}

        ${baseClassDef}

        ${constructorClassDefs}
    `;
}

function makeStaticConstructor(p: PatternDefinition, c: PatternConstructor): string {
    const funcArgs = constructorArgs(c);
    const generics = makeGenericsTemplate(p.generics);
    const funcName = c.name;
    const argVariables = argNames(c).join(', ');
    const baseClass = baseClassName(p);
    return `
        static ${funcName}${generics}(${funcArgs}): ${baseClass}${generics} {
            return new ${funcName}(${argVariables});
        }
    `;
}

function makeStaticConstructors(p: PatternDefinition): string {
    return p.constructors.map(x => makeStaticConstructor(p, x)).join('\n\n');
}

export function baseClass(p: PatternDefinition): string {
    const className = baseClassName(p);
    const generics = makeGenericsTemplate(p.generics);
    const staticConstructors = makeStaticConstructors(p);
    const classStr = `
        export class ${className}${generics} extends PatternMatchable {
            ${baseClassConstructor(p)}

            ${staticConstructors}
            
            ${matchFunctionDef(p)}

            ${p.extraDefs}
        }
    `
    return classStr;
}

function constructorClassDef(p: PatternDefinition, c: PatternConstructor): string {
    const baseClass = baseClassName(p);
    const className = c.name;
    const generics = makeGenericsTemplate(p.generics);
    const vars = makeVariableDefs(c);
    const conArgs = constructorArgs(c);
    const readonlyProps = vars.map(({name, type}) => `readonly ${name}: ${type};`).join('\n');
    const argsAssignments = vars.map(({name}) => `this.${name} = ${name};`).join('\n');
    const onMatchFuncType = makeFuncTypeSig(c, 'any');
    const onMatchFuncAppVariables = vars.map(({name}) => `this.${name}`).join(', ');
    const onMatchDef = `this.onMatch = (f: ${onMatchFuncType}) => f(${onMatchFuncAppVariables});`
    const constructor = `
        constructor(${conArgs}) {
            super();
            ${argsAssignments}
            ${onMatchDef}
        }
    `;
    const classStr = `
        class ${className}${generics} extends ${baseClass}${generics} {
            ${readonlyProps}
            ${constructor}
        }
    `;
    return classStr;
}

const retType = '_ret';

// function genericsWithRet(p: PatternDefinition): string {
//     return iGenTemplate(p);
// }

function iGenTemplate(p: PatternDefinition): string {
    const iGenerics = p.generics.concat(retType);
    const iGenTemplate = makeGenericsTemplate(iGenerics);
    return iGenTemplate;
}

function fullPatternName(p: PatternDefinition): string {
    return `Full${p.name}Pattern`;
}

function fullPatternNameWithGenerics(p: PatternDefinition): string {
    return `${fullPatternName(p)}${iGenTemplate(p)}`;
}

function patternName(p: PatternDefinition): string {
    return `${p.name}Pattern`;
}

function patternNameWithGenerics(p: PatternDefinition): string {
    return `${patternName(p)}${iGenTemplate(p)}`;
}

function baseClassName(p: PatternDefinition): string {
    return `${p.name}`;
}

function matchFunctionDef(p: PatternDefinition): string {
    const tName = patternNameWithGenerics(p);
    return `
        match<${retType}>(patterns: ${tName}): ${retType} {
            return this.on(patterns as unknown as StringsToFunctions<${retType}>);
        }
    `;
}



function makeFullPatternInterfaceDef(p: PatternDefinition): string {
    const params = p.constructors.map(x => `${x.name}: ${makeFuncTypeSig(x, retType)};`);
    return `
        export interface ${fullPatternNameWithGenerics(p)} {
            ${params.join("\n")}
        }
    `;
}

function makePatternTypeDef(p: PatternDefinition): string {
    const iName = fullPatternNameWithGenerics(p);
    const tName = patternNameWithGenerics(p);
    return `
        export type ${tName} = Pattern<${iName}, ${retType}>;
    `;
}

export type Pattern<t, a> = t | (Partial<t> & WildcardPattern<a>)

export interface StringsToFunctions<a> {
    [x: string]: (...args: any[]) => a
}

export class PatternMatchable {
    private ctrs: any[];
    private options: string[];
    private T: string;

    constructor(ctrs: any[]) {
        this.ctrs = ctrs;
        /* eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-assignment */
        this.options = this.ctrs.map(x => x.name);
        this.T = this.constructor.name;
    }

    _isExhaustive<a>(patterns: StringsToFunctions<a>): null | Error {
        const patternNames = Object.entries(patterns).map(([p, _]) => p);
        if (patternNames.includes(WILDCARD)) {
            // wildcard is always completely exhaustive
            return null;
        }

        const failures = this.options.filter(opt => !patternNames.includes(opt));
        if (failures.length === 0) {
            return null;
        }

        return new Error(`Could not find patterns for ${failures.join(', ')}`);
    }

    _containsUnknowns<a>(patterns: StringsToFunctions<a>): null | Error {
        const patternNames = Object.entries(patterns).map(([p, _]) => p)
        const failures = patternNames.filter(p => p !== WILDCARD && !this.options.includes(p))
        if (failures.length === 0) {
            return null;
        }

        return new Error(`Excess patterns found: ${failures.join(', ')}`)
    }

    on<a>(patterns: StringsToFunctions<a>): a {
        const errs = new ErrorList(
            this._isExhaustive(patterns),
            this._containsUnknowns(patterns)
        );
        if (!errs.empty) {
            throw errs;
        }
        const entries = Object.entries(patterns);
        const foundMatches = entries.filter(([key, _]) => this.matches(key));
        const specificMatch = foundMatches.find(([key, _]) => key !== WILDCARD);
        let func;
        if (specificMatch === undefined) {
            // use wildcard one
            // which should be the only one in the list
            func = foundMatches[0][1];
        } else {
            func = specificMatch[1];
        }

        return this.onMatch(func);
    }

    /**
     * @param {Function<a, b>} _
     * @return {b}
     */
    onMatch<a,b>(_: (...args: any[]) => b): b {
        throw new Error(`UNDEFINED onMatch() for ${this.T}!!!`);
    }

    matches(patternName: string): boolean  {
        return patternName === this.T || patternName === WILDCARD;
    }
}

