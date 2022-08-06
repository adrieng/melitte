%{ (* -*- mode: tuareg -*- *)
   open Raw

   let v = Position.value
%}

%token<string> ID

%token LAM FORALL LET IN TYPE NAT ZERO SUC ELIM WITH VAL EVAL
%token LPAREN RPAREN LBRACE RBRACE
%token EQ ARR DARR
%token UNDERSCORE COLON BAR COMMA
%token EOF

%nonassoc IN DARR
%right ARR

%start<Raw.t> file

%%

%inline parens(X):
| LPAREN x = X RPAREN { x }

%inline located(X):
| x = X { Position.with_poss $startpos $endpos x }

%inline name:
| id = ID { id }

very_simple_term_:
| id = name { Var id }
| TYPE { Type }
| NAT { Nat }
| ZERO { Zero }
| SUC t = very_simple_term { Suc t }
| te = parens(term_) { te }

%inline very_simple_term:
| located(very_simple_term_) { $1 }

simple_term_:
| te = very_simple_term_ { te }
| f = simple_term a = very_simple_term { App (f, a) }

%inline simple_term:
| located(simple_term_) { $1 }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pattern DARR ty = ty { Build.bound1 p ty }

term_:
| t = simple_term_ { t }
| LAM ids = pattern+ DARR t = term { v @@ Build.lambda ids t }
| LET p = pattern COLON ty = ty EQ def = term IN body = term
  { Let { def; ty; body = Build.bound1 p body; } }
| FORALL hyps = parens(hyp)+ ARR b = ty { v @@ Build.forall hyps b }
| a = term ARR b = term { v @@ Build.arrow a b }
| ELIM scrut = term motive = motive
  LBRACE
  BAR? ZERO DARR case_zero = term
  BAR SUC case_succ = bind2(DARR)
  RBRACE { Natelim { scrut; motive; case_zero; case_succ; } }

%inline term:
| located(term_) { $1 }

%inline ty: term { $1 }

bind1(SEP):
| p = pattern SEP t = term { Build.bound1 p t }

bind2(SEP):
| p1 = pattern COMMA p2 = pattern SEP t = term { Build.bound2 p1 p2 t }

pattern_:
| UNDERSCORE { PWildcard }
| id = ID { PVar id }

%inline pattern:
| located(pattern_) { $1 }

hyp:
| p = pattern COLON ty = ty { (p, ty) }

phrase_desc:
| VAL name = name COLON ty = ty EQ body = term { Val { name; ty; body; } }
| EVAL body = term COLON ty = ty { Eval { body; ty; } }

%inline phrase:
| p = located(phrase_desc) { p }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
