%{
    open Raw
%}

%token<string> ID

%token LAM FORALL LET IN TYPE NAT ZERO SUCC ELIM AS RETURN VAL
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
| s = simple_term ARR t = simple_term { Forall ({ id = None; ann = Some s; }, t) }
| te = parens(term) { te }

weakened_term(X):
| b = binding X te = term { (b, te) }

term:
| te = simple_term { te }
| LAM wte = weakened_term(ARR) { Lam wte }
| f = simple_term a = term { App (f, a) }
| FORALL wte = weakened_term(ARR) { Forall wte }
| LET b = binding EQ t = term IN u = term { Let (t, (b, u)) }
| SUCC te = term { Succ te }
| ELIM te = term
  AS id = pat RETURN ty = typ
  LBRACE teZ = term
  BAR wte = weakened_term(DARR)
  RBRACE { Natelim (te, (id, ty), teZ, wte) }

typ: simple_term { $1 }

weakened_typ(X):
| b = binding X ty = typ { (b, ty) }

pat:
| UNDERSCORE { None }
| id = ID { Some id }

binding:
| id = pat { { id; ann = None; } }
| id = pat COLON ty = typ { { id; ann = Some ty; } }

phrase:
| VAL b = binding EQ te = term { Val (b, te) }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
