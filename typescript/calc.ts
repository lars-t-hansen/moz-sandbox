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

module calc
{
    export interface VEnvMap {
	[index:string]: number
    }

    export class VEnv {
	private env:  VEnvMap;
	private outer:VEnv;
	constructor(env:VEnvMap, outer:VEnv=null) {
	    this.env = env;
	    this.outer = outer;
	}
	public update(name:string, value:number):void {
	    this.env[name] = value;
	}
	public lookup(name:string): number {
	    if (this.env.hasOwnProperty(name))
		return this.env[name];
	    if (this.outer)
		return this.outer.lookup(name);
	    throw "Unknown variable: " + name;
	}
    }
	
    export interface FEnv {
	[index:string]: { arity:number; code: (venv:VEnv, fenv:FEnv, actuals:number[]) => number }
    }

    export function run(venv: VEnv, fenv: FEnv, program: string): number {
	return evaluate(venv, fenv, parse(lex(program)));
    }

    // Evaluator

    function evaluate(venv: VEnv, fenv: FEnv, t: Tree) : number {
	switch (t.tag) {
	  case Ast.Defn: {
	      var d = <Defn> t;
	      fenv[d.name] = { arity: d.formals.length,
			       code: makeFunction(d.formals, d.body) };
	      return 0;
	  }
	  case Ast.Assign: {
	      var a = <Assign> t;
	      var v = evaluate(venv, fenv, a.val);
	      venv.update(a.name, v);
	      return v;
	  }
	  case Ast.Call: {
	      var c = <Call> t;
	      if (!fenv.hasOwnProperty(c.name))
		  throw "Unknown function: " + c.name;
	      var f = fenv[c.name];
	      if (f.arity != c.args.length)
		  throw "Wrong number of arguments to " + c.name;
	      var vs = c.args.map(function (x) { return evaluate(venv, fenv, x); });
	      return f.code(venv, fenv, vs);
	  }
	  case Ast.Unop: {
	      var u = <Unop> t;
	      var o = evaluate(venv, fenv, u.opd);
	      switch (u.opr) {
		case UnOpr.Negate: return -o;
		default:           throw "Bad unop";
	      }
	  }
	  case Ast.Binop: {
	      var b = <Binop> t;
	      var l = evaluate(venv, fenv, b.lhs);
	      var r = evaluate(venv, fenv, b.rhs);
	      switch (b.opr) {
		case BinOpr.Plus:   return l+r;
		case BinOpr.Minus:  return l-r;
		case BinOpr.Times:  return l*r;
		case BinOpr.Divide: return l/r;
		default:            throw "Bad binop";
	      }
	  }
	  case Ast.VarRef: {
	      return venv.lookup((<VarRef> t).name);
	  }
	  case Ast.NumLit: {
	      return (<NumLit> t).value;
	  }
	  default:
	    throw "Bad ast";
	}
    }

    function makeFunction(formals:string[], body:Tree): (venv:VEnv, fenv:FEnv, actuals:number[]) => number {
	return function (venv, fenv, actuals) {
	    var newrib = <VEnvMap> {};
	    for ( var i=0 ; i < formals.length ; i++ )
		newrib[formals[i]] = actuals[i];
	    return evaluate(new VEnv(newrib, venv), fenv, body)
	}
    }

    // Parser

    enum Ast { Unop, Binop, VarRef, NumLit, Assign, Call, Defn };
    enum BinOpr { Plus, Minus, Times, Divide };
    enum UnOpr { Negate };

    class Tree { constructor(public tag:Ast) {} }
    class Unop extends Tree { constructor(public opr:UnOpr, public opd:Tree) { super(Ast.Unop) } }
    class Binop extends Tree { constructor(public opr:BinOpr, public lhs:Tree, public rhs:Tree) { super(Ast.Binop) } }
    class Assign extends Tree { constructor(public name:string, public val:Tree) { super(Ast.Assign); } }
    class VarRef extends Tree { constructor(public name:string) { super(Ast.VarRef) } }
    class NumLit extends Tree { constructor(public value:number) { super(Ast.NumLit) } }
    class Call extends Tree { constructor(public name:string, public args:Tree[]) { super(Ast.Call) } }
    class Defn extends Tree { constructor(public name:string, public formals:string[], public body:Tree) { super(Ast.Defn) } }

    function parse(ts:Token[]): Tree {
	var e = parseStmt(ts);
	if (ts.length != 0)
	    throw "Input not consumed: " + ts.length;
	return e;
    }

    function parseStmt(ts:Token[]): Tree {
	if (eatIdent(ts, "fn")) {
	    if (peek(ts) != Tok.Ident)
		throw "Expected function name";
	    var name = (<Ident>ts[0]).name;
	    ts.shift();
	    match(ts, Tok.LeftParen);
	    var formals:string[] = [];
	    var t:Tok;
	    while ((t = peek(ts)) != Tok.EOI && t != Tok.RightParen) {
		if (formals.length)
		    match(ts, Tok.Comma);
		if (peek(ts) != Tok.Ident)
		    throw "Expected parameter name";
		var param = (<Ident>ts[0]).name;
		ts.shift();
		formals.push(param);
	    }
	    match(ts, Tok.RightParen);
	    var body = parse0(ts);
	    return new Defn(name, formals, body);
	}
	else {
	    var e = parse0(ts);
	    if (peek(ts) == Tok.Assign) {
		if (e.tag != Ast.VarRef)
		    throw "Left-hand-side of '=' must be identifier";
		var name = (<VarRef> e).name;
		ts.shift();
		e = new Assign(name, parse0(ts));
	    }
	    return e;
	}
    }

    function parse0(ts:Token[]): Tree {
	var e = parse1(ts);
	for (;;) {
	    var t:Tok;
	    if ((t = peek(ts)) != Tok.Plus && t != Tok.Minus)
		break;
	    ts.shift();
	    var e2 = parse1(ts);
	    e = new Binop(translate(t), e, e2);
	}
	return e;
    }

    function parse1(ts:Token[]): Tree {
	var e = parse2(ts);
	for (;;) {
	    var t:Tok;
	    if ((t = peek(ts)) != Tok.Times && t != Tok.Divide)
		break;
	    ts.shift();
	    var e2 = parse2(ts);
	    e = new Binop(translate(t), e, e2);
	}
	return e;
    }

    function parse2(ts:Token[]): Tree {
	switch (peek(ts)) {
	  case Tok.Minus: {
	      ts.shift();
	      var e = parse2(ts);
	      return new Unop(UnOpr.Negate, e);
	  }
	  case Tok.LeftParen: {
	      ts.shift();
	      var e = parse0(ts);
	      match(ts, Tok.RightParen);
	      return e;
	  }
	  case Tok.Ident: {
	      var name = (<Ident> ts[0]).name;
	      ts.shift();
	      if (eat(ts, Tok.LeftParen)) {
		  var t:Tok;
		  var args = <Tree[]> [];
		  while ((t = peek(ts)) != Tok.EOI && t != Tok.RightParen) {
		      if (args.length)
			  match(ts, Tok.Comma);
		      args.push(parse1(ts));
		  }
		  match(ts, Tok.RightParen);
		  return new Call(name, args);
	      }
	      else
		  return new VarRef(name);
	  }
	  case Tok.Number: {
	      var value = (<Num> ts[0]).value;
	      ts.shift();
	      return new NumLit(value);
	  }
	  case Tok.EOI:
	    throw "End of input";
	  default:
	    throw "Bad token!";
	}
    }

    function eatIdent(ts:Token[], id:string): boolean {
	if (peek(ts) == Tok.Ident && (<Ident> ts[0]).name == id) {
	    ts.shift();
	    return true;
	}
	return false;
    }

    function match(ts:Token[], t:Tok) {
	if (!eat(ts, t))
	    throw "Error: expected " + Tok[t];
    }

    function eat(ts:Token[], t:Tok): boolean {
	if (peek(ts) == t) {
	    ts.shift();
	    return true;
	}
	return false;
    }

    function translate(t:Tok):BinOpr {
	switch (t) {
	  case Tok.Plus:   return BinOpr.Plus;
	  case Tok.Minus:  return BinOpr.Minus;
	  case Tok.Times:  return BinOpr.Times;
	  case Tok.Divide: return BinOpr.Divide;
	  default:         throw "Bad token";
	}
    }

    function peek(ts:Token[]): Tok {
	return ts.length == 0 ? Tok.EOI : ts[0].tag;
    }

    // Lexer

    enum Tok { Number, Ident, Plus, Minus, Times, Divide, LeftParen, RightParen, Assign, Comma, EOI };

    class Token { constructor(public tag:Tok) {} }
    class Num extends Token { constructor(public value:number) { super(Tok.Number) } }
    class Ident extends Token { constructor(public name:string) { super(Tok.Ident) } }

    function lex(input:string) : Token[]  {
	var i = 0;
	var ts:Token[] = [];
	while (i < input.length) {
	    var c = input.charAt(i++);
	    switch (c) {
	      case ' ':
	      case '\t':
	      case '\n':
	      case '\r':
		break;
	      case '+':
		ts.push(new Token(Tok.Plus));
		break;
	      case '-':
		ts.push(new Token(Tok.Minus));
		break;
	      case '*':
		ts.push(new Token(Tok.Times));
		break;
	      case '/':
		ts.push(new Token(Tok.Divide));
		break;
	      case '(':
		ts.push(new Token(Tok.LeftParen));
		break;
	      case ')':
		ts.push(new Token(Tok.RightParen));
		break;
	      case '=':
		ts.push(new Token(Tok.Assign));
		break;
	      case ',':
		ts.push(new Token(Tok.Comma));
		break;
	      default:
		if (isDigit(c)) {
		    var s = c;
		    while (i < input.length && isDigit(c = input.charAt(i))) {
			s += c;
			i++;
		    }
		    ts.push(new Num(parseFloat(s)));
		}
		else if (isLetter(c)) {
		    var s = c;
		    while (i < input.length && isLetter(c = input.charAt(i))) {
			s += c;
			i++;
		    }
		    ts.push(new Ident(s));
		}
		else
		    throw "Syntax Error @ position " + i;
		break;
	    }
	}
	return ts;
    }

    function isLetter(c:string):boolean {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';
    }

    function isDigit(c:string):boolean {
	return c >= '0' && c <= '9';
    }

}
