// Simple desk calculator, each line in stdin is an expression,
// it is evaluated and its value printed
//
// Double-precision numbers
// Infix + - * / with usual precedence
// Prefix -
// Parens
//
// To make this interesting, we split lexing, parsing, and evaluation
// into three passes

fn main() {
  for line in io::stdin().lines() {
    let txt = line.unwrap();
    let ts = lex(txt);
    let tree = parse(ts);
    let result = eval(tree);
    print!("{}", result);
  }      
}

enum Tree {
  Neg(Tree),
  Plus(Tree, Tree),
  Minus(Tree, Tree),
  Times(Tree, Tree),
  Divide(Tree, Tree),
  Literal(f64)
}

fn eval(t: Tree) -> f64 {
  match t {
    Plus(x, y) => { return eval(x) + eval(y); }
    Minus(x, y) => { return eval(x) - eval(y); }
    Times(x, y) => { return eval(x) * eval(y); }
    Divide(x, y) => { return eval(x) / eval(y); }
    Neg(x) => { return -eval(x); }
    Literal(x) => { return x; }
  }
}

fn parse(ts: Vec<Token>) -> Tree {
}

enum Token {
  LParen,
  RParen,
  Plus,
  Minus,
  Times,
  Divide,
  Literal(f64)
}

enum State {
  Start,        // intertoken
  LitInt,       // seen digit in integer part
  LitFracStart, // seen ., must have fraction
  LitFrac,      // seen digit in fraction
  LitExpStart,  // seen e or E, need sign or digits
  LitExpStart2, // seen e or E and + or -, need digits
  LitExp        // seen digits in exponent
}

fn lex(txt:&str) -> Vec<Token> {
  let mut state = Start;
  let mut tokens = new Vec<Token>();
  let mut buffer = new Vec<char>();
  for ch in txt.as_slice().chars() {
    match state {
      Start => {
        match ch {
          '(' => { tokens.push(LParen); }
          ')' => { tokens.push(RParen); }
          '+' => { tokens.push(Plus); }
          '-' => { tokens.push(Minus); }
          '*' => { tokens.push(Times); }
          '/' => { tokens.push(Divide); }
          '.' => { buffer.push(ch); state=LitFracStart; }
          '0' | '1' | '2' | '3' | ... | '9' => { buffer.push(ch); state=LitInt; }
          _ => { syntaxError(); }
        }
      }
      LitInt => {
        match ch {
          '(' => { tokens.push(lit(buffer)); tokens.push(LParen); state=Start; }
          ')' => { ...; tokens.push(RParen); }
          '+' => { tokens.push(Plus); }
          '-' => { tokens.push(Minus); }
          '*' => { tokens.push(Times); }
          '/' => { tokens.push(Divide); }
          '.' => { buffer.push(ch); state=LitFracStart; }
          '0' | '1' | '2' | '3' | ... | '9' => { buffer.push(ch); state=LitInt; }
          _ => { syntaxError(); }
    if ch.is_whitespace() {
      match state {
        Start => {}
        _ => { tokens.push(grabLiteral(buffer)); state = Start; }
      }
    }
    else if ch == '(' {
      match state {
        Start => { tokens.push(LParen); }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => syntaxError();
      }
    }
    else if ch == ')' {
      match state {
        Start => { tokens.push(RParen); }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => syntaxError();
      }
    }
    else if ch == '+' {
      match state {
        Start => { tokens.push(Plus); }
        LitExpStart => { buffer.push(ch); state = LitExpStart2; }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => { syntaxError(); }
      }
    }
    else if ch == '-' {
      match state {
        Start => { tokens.push(Minus); }
        LitExpStart => { buffer.push(ch); state = LitExpStart2; }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => { syntaxError(); }
      }
    }
    else if ch == '*' {
      match state {
        Start => { tokens.push(Multiply); }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => { syntaxError(); }
      }
    }
    else if ch == '/' {
      match state {
        Start => { tokens.push(Divide); }
	LitInt | LitFrac | LitExp => { tokens.push(grabLiteral(buffer)); state=Start; }
        _ => { syntaxError(); }
      }
    }
    else if ch >= '0' && ch <= '9' {
      match state {
        Start => { buffer.push(ch); state = LitInt; }
        LitInt => { buffer.push(ch); }
        LitFracStart => { buffer.push(ch); state = LitFrac; }
        LitFrac => { buffer.push(ch); }
        LitExpStart => { buffer.push(ch); state = LitExp; }
        LitExpStart2 => { buffer.push(ch); state = LitExp; }
        LitExp => { buffer.push(ch); }
      }
    }
    else if ch == '.' {
      match state {
        Start => { buffer.push(ch); state = LitFracStart; }
	LitInt => { buffer.push(ch); state = LitFracStart; }
        _ => { syntaxError() }
      }
    }
  }
  match state {
    Start => { return tokens; }
    _ => { syntaxError(); }
  }
}
