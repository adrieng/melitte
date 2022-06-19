%{ (* -*- mode: tuareg -*- *)
    open Raw

    let mk_lam ids body =
      (List.fold_right lam ids body).Position.value

    let mk_forall hyps body =
      (List.fold_right forall hyps body).Position.value

    let mk_arr a b =
      Forall (a, ({ value = None; position = a.position; }, b))
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
| FORALL hyps = parens(hyp)+ ARR b = ty { mk_forall hyps b }
| a = ty ARR b = ty { mk_arr a b }
| te = parens(term_) { te }

simple_term:
| located(simple_term_) { $1 }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pat DARR ty = ty { (p, ty) }

term_:
| t = simple_term_ { t }
| LAM ids = pat+ ARR t = term { mk_lam ids t }
| f = simple_term a = term { App (f, a) }
| LET p = pat COLON ty = ty EQ bound = term IN body = term
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
| x = pat SEP t = term { (x, t) }

pat_:
| UNDERSCORE { None }
| id = ID { Some id }

pat:
| located(pat_) { $1 }

hyp:
| p = pat COLON ty = ty { (p, ty) }

phrase:
| VAL id = name COLON ty = ty EQ t = term { Val (id, ty, t) }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
