%{ (* -*- mode: tuareg -*- *)
    open Raw

    let mk_lam ids body =
      List.fold_right (fun id body -> Lam (id, body)) ids body

    let mk_forall hyps body =
      List.fold_right (fun (p, a) b -> Forall (a, (p, b))) hyps body

    let mk_arr a b =
      Forall (a, (None, b))
%}

%token<string> ID

%token LAM FORALL LET IN TYPE NAT ZERO SUCC ELIM WITH VAL
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EQ ARR DARR
%token UNDERSCORE COLON BAR
%token EOF

%right ARR

%start<Raw.t> file

%%

parens(X):
| LPAREN x = X RPAREN { x }

%inline name:
| id = ID { id }

simple_term:
| id = name { Var id }
| TYPE { Type }
| NAT { Nat }
| ZERO { Zero }
| SUCC { Succ }
| FORALL hyps = parens(hyp)+ ARR b = ty { mk_forall hyps b }
| a = ty ARR b = ty { mk_arr a b }
| te = parens(term) { te }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pat DARR ty = ty { (p, ty) }

term:
| te = simple_term { te }
| LAM ids = pat+ ARR te = term { mk_lam ids te }
| f = simple_term a = term { App (f, a) }
| LET p = pat COLON ty = ty EQ bound = term IN body = term
  { Let { bound; ty; body = (p, body); } }
| ELIM discr = term motive = motive?
  LBRACE
  BAR? ZERO DARR case_zero = term
  BAR SUCC case_succ = binding(DARR)
  RBRACE { Natelim { discr; motive; case_zero; case_succ; } }

ty: simple_term { $1 }

binding(SEP):
| x = pat SEP t = term { (x, t) }

pat:
| UNDERSCORE { None }
| id = ID { Some id }

hyp:
| p = pat COLON ty = ty { (p, ty) }

phrase:
| VAL id = name COLON ty = ty EQ t = term { Val (id, ty, t) }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
