// Canonical calculator example, for use from within the JS shell.
// (Interactive input is line-oriented.)
//
// Syntax:
//
// stmt  ::= id = expr
//         | "fn" id "(" ids ")" expr
//         | expr
// ids   ::= ( id ("," id)* )?
// expr  ::= expr binop expr
//         | unop expr
//         | "(" expr ")"
//         | id "(" exprs ")"
//         | num
//         | id
// exprs ::= ( expr ("," expr)* )?
// binop ::= "+" | "-" | "*" | "/"
// unop  ::= "-"

declare function readline () : string;
declare function print (datum:any) : void;

module calc.main
{
    import VEnv = calc.VEnv;
    import FEnv = calc.FEnv;

    var venv = new VEnv({ "PI": Math.PI });
    var fenv: FEnv = { "sin": { arity: 1, code: function (env, fenv, args) { return Math.sin(args[0]) } },
		       "cos": { arity: 1, code: function (env, fenv, args) { return Math.cos(args[0]) } },
		       "pow": { arity: 2, code: function (env, fenv, args) { return Math.pow(args[0], args[1]) } } };

    var s:string;
    while (s = readline()) {
	try {
	    print(calc.run(venv, fenv, s));
	}
	catch (e) {
	    print(e);
	}
    }
}
