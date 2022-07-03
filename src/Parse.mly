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

%nonassoc IN DARR
%right ARR
%nonassoc APP

%start<Raw.t> file

%%

%inline parens(X):
| LPAREN x = X RPAREN { x }

%inline located(X):
| x = X { Position.with_poss $startpos $endpos x }

%inline name:
| id = ID { id }

simple_term_:
| id = name { Var id }
| TYPE { Type }
| NAT { Nat }
| ZERO { Zero }
| SUCC { Succ }
| te = parens(term_) { te }

%inline simple_term:
| located(simple_term_) { $1 }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pattern DARR ty = ty { (p, ty) }

term_:
| t = simple_term_ { t }
| LAM ids = pattern+ DARR t = term { v @@ Build.lambda ids t }
| f = simple_term a = term %prec APP { App (f, a) }
| LET p = pattern COLON ty = ty EQ bound = term IN body = term
  { Let { bound; ty; body = (p, body); } }
| FORALL hyps = parens(hyp)+ ARR b = ty { v @@ Build.forall hyps b }
| a = term ARR b = term { v @@ Build.arrow a b }
| ELIM discr = term motive = motive?
  LBRACE
  BAR? ZERO DARR case_zero = term
  BAR SUCC case_succ = binding(DARR)
  RBRACE { Natelim { discr; motive; case_zero; case_succ; } }

%inline term:
| located(term_) { $1 }

%inline ty: term { $1 }

binding(SEP):
| x = pattern SEP t = term { (x, t) }

pattern_:
| UNDERSCORE { PWildcard }
| id = ID { PVar id }

%inline pattern:
| located(pattern_) { $1 }

hyp:
| p = pattern COLON ty = ty { (p, ty) }

phrase_desc:
| VAL id = name COLON ty = ty EQ t = term { Val (id, ty, t) }

%inline phrase:
| p = located(phrase_desc) { p }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
