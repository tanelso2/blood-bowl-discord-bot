import { PatternConstructor, PatternDefinition } from './lang';

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

function makeIsTypeGuardFunc(c: PatternConstructor, p: PatternDefinition): string {
    const { name } = c;
    const generics = makeGenericsTemplate(p.generics);
    return `
        is${name}(): this is ${name}${generics} {
            return this instanceof ${name};
        }
    `;
}

function makeIsTypeGuardFuncs(p: PatternDefinition): string {
    return p.constructors.map(x => makeIsTypeGuardFunc(x, p)).join('\n\n');
}

function baseClass(p: PatternDefinition): string {
    const className = baseClassName(p);
    const generics = makeGenericsTemplate(p.generics);
    const staticConstructors = makeStaticConstructors(p);
    const isTypeGuardFuncs = makeIsTypeGuardFuncs(p);
    const classStr = `
        export class ${className}${generics} extends PatternMatchable {
            ${baseClassConstructor(p)}

            ${staticConstructors}
            
            ${matchFunctionDef(p)}
            
            ${isTypeGuardFuncs}

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

export function patternDefToTS(p: PatternDefinition): string {
    const fullPatternInterfaceDef = makeFullPatternInterfaceDef(p);
    const patternTypeDef = makePatternTypeDef(p);
    const baseClassDef = baseClass(p);
    const constructorClassDefs = p.constructors.map(x => constructorClassDef(p, x)).join('\n\n');
    return `
        /* eslint-disable @typescript-eslint/no-explicit-any */
        /* eslint-disable @typescript-eslint/no-unused-vars */
        /* eslint-disable @typescript-eslint/no-unsafe-return */
        /* eslint-disable @typescript-eslint/no-unsafe-assignment */
        import { PatternMatchable, Pattern, StringsToFunctions } from '@core/types/patterns/pattern';

        ${fullPatternInterfaceDef}
         
        ${patternTypeDef}

        ${baseClassDef}

        ${constructorClassDefs}
    `;
}