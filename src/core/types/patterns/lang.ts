import * as Pars from 'parsimmon';

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

export function parseCharsUntil(x: Pars.Parser<any>): Pars.Parser<string> {
    // p will either consume a character or fail if the lookahead is found.
    const p = Pars.lookahead(x).or(Pars.any).assert(x => x !== '', 'found the lookahead');
    // calling p.many() means it will consume until failing (which is when the lookahead is found)
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

export function anythingSurroundedBy(lbound: Pars.Parser<any>, rbound: Pars.Parser<any>): Pars.Parser<string> {
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