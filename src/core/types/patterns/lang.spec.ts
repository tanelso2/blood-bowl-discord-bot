import { patternLanguage } from "./lang";

const boolDef = `
pat Bool(
    True
    | False
)
{|
    static ofBoolean(b: boolean): Bool {
        return b ? this.True() : this.False();
    }
|}
`;

const simpleDef = `
pat Simple(
    Foo
    | Bar number
)
{|
|}
`;

const genericDef = `
pat Either<a,b> (
    Left a
    | Right b
)`;

describe('Parsing?', () => {
    it('should parse simpleDef', () => {
        const simpl = patternLanguage.patternDef.tryParse(simpleDef);
        simpl.name.should.eql('Simple');
        simpl.constructors.length.should.eql(2);
        const foo = simpl.constructors.find(x => x.name === 'Foo')!;
        foo.args.length.should.eql(0);
        const bar = simpl.constructors.find(x => x.name === 'Bar')!;
        bar.args.length.should.eql(1);
        bar.args[0].should.eql('number');
    })

    it('should parse genericDef', () => {
        const gen = patternLanguage.patternDef.tryParse(genericDef);
        gen.name.should.eql('Either');
        gen.generics.length.should.eql(2);
        gen.generics.find(x => x === 'a')!.should.not.be.undefined;
        gen.generics.find(x => x === 'b')!.should.not.be.undefined;
        gen.extraDefs.should.eql('');
        gen.constructors.length.should.eql(2);
        const left = gen.constructors.find(x => x.name === 'Left')!;
        left.args.length.should.eql(1);
        left.args[0].should.eql('a');
        const right = gen.constructors.find(x => x.name === 'Right')!;
        right.args.length.should.eql(1);
        right.args[0].should.eql('b');

    });

    it('should parse ctors', () => {
        patternLanguage.ctorList.tryParse(`(True | False)`);
        patternLanguage.generics.tryParse(`<a>`);
        patternLanguage.extraDefs.tryParse(`{| |}`);
        patternLanguage.extraDefs.tryParse(`{||}`);
    })

    it('should read the Bool pattern', () => {
        const pat = patternLanguage.patternDef.tryParse(boolDef);
        pat.name.should.eql('Bool');
        pat.generics.should.be.empty;
        pat.extraDefs.length.should.be.greaterThan(3);
    });
})