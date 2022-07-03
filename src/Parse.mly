%{ (* -*- mode: tuareg -*- *)
   open Raw

   let v = Position.value
%}

%token<string> ID

%token LAM FORALL LET IN TYPE NAT ZERO SUCC ELIM WITH VAL
%token LPAREN RPAREN LBRACE RBRACE
%token EQ ARR DARR
%token UNDERSCORE COLON BAR
%token EOF

%right ARR

%start<Raw.t> file

%%

parens(X):
| LPAREN x = X RPAREN { x }

located(X):
| x = X { Position.with_poss $startpos $endpos x }

%inline name:
| id = ID { id }

simple_term_:
| id = name { Var id }
| TYPE { Type }
| NAT { Nat }
| ZERO { Zero }
| SUCC { Succ }
| FORALL hyps = parens(hyp)+ ARR b = ty { v @@ Build.forall hyps b }
| a = ty ARR b = ty { v @@ Build.arrow a b }
| te = parens(term_) { te }

simple_term:
| located(simple_term_) { $1 }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pattern DARR ty = ty { (p, ty) }

term_:
| t = simple_term_ { t }
| LAM ids = pattern+ DARR t = term { v @@ Build.lambda ids t }
| f = simple_term a = term { App (f, a) }
| LET p = pattern COLON ty = ty EQ bound = term IN body = term
  { Let { bound; ty; body = (p, body); } }
| ELIM discr = term motive = motive?
  LBRACE
  BAR? ZERO DARR case_zero = term
  BAR SUCC case_succ = binding(DARR)
  RBRACE { Natelim { discr; motive; case_zero; case_succ; } }

term:
| located(term_) { $1 }

ty: simple_term { $1 }

binding(SEP):
| x = pattern SEP t = term { (x, t) }

pattern_:
| UNDERSCORE { PWildcard }
| id = ID { PVar id }

pattern:
| located(pattern_) { $1 }

hyp:
| p = pattern COLON ty = ty { (p, ty) }

phrase_desc:
| VAL id = name COLON ty = ty EQ t = term { Val (id, ty, t) }

phrase:
| p = located(phrase_desc) { p }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
