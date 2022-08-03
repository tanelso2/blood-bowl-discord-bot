pat Bool ( 
    True
    | False
)
{|
    static ofBoolean(b: boolean): Bool {
        return b ? this.True() : this.False();
    }
|}