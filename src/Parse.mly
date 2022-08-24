%{ (* -*- mode: tuareg -*- *)
   open Raw

   module B = Build
%}

%token<string> ID
%token<int> INT

%token LAM FORALL SIGMA LET IN TYPE NAT ZERO SUC ELIM WITH VAL EVAL FST SND
%token LPAREN RPAREN LBRACE RBRACE
%token EQ ARR DARR TIMES
%token UNDERSCORE COLON BAR COMMA
%token EOF

%nonassoc IN DARR
%right ARR
%right TIMES

%start<Raw.t> file

%%

%inline parens(X):
| LPAREN x = X RPAREN { x }

%inline located(X):
| x = X { x ~loc:(Position.lex_join $startpos $endpos) () }

%inline name:
| id = ID { Name.of_string id }

very_simple_term_:
| name = name { B.var ~name }
| TYPE level = INT { B.typ ~level }
| NAT { B.nat }
| k = INT { B.lit ~k }
| ZERO { B.zero }
| SUC t = very_simple_term { B.suc ~t }
| FST arg = very_simple_term { B.fst ~arg }
| SND arg = very_simple_term { B.snd ~arg }
| te = parens(term_) { te }

%inline very_simple_term:
| located(very_simple_term_) { $1 }

simple_term_:
| te = very_simple_term_ { te }
| func = simple_term arg = very_simple_term { B.app ~func ~arg }

%inline simple_term:
| located(simple_term_) { $1 }

weakened_term(X):
| b = hyp X te = term { (b, te) }

motive:
| WITH p = pattern DARR ty = ty { B.bound1 p ty }

term_:
| t = simple_term_
  { t }
| LAM params = pattern+ DARR body = term
  { B.lam_n ~params ~body }
| LET p = pattern COLON ty = ty EQ def = term IN body = term
  { B.let_ ~def ~ty ~body:(B.bound1 p body) }
| FORALL params = parens(hyp)+ ARR body = ty
  { B.pi_n ~params ~body }
| SIGMA params = parens(hyp)+ TIMES body = ty
  { B.sigma_n ~params ~body }
| dom = term ARR cod = term
  { B.arrow ~dom ~cod }
| left = term TIMES right = term
  { B.product ~left ~right }
| ELIM scrut = term motive = motive
  LBRACE
  BAR? ZERO DARR case_zero = term
  BAR SUC case_suc = bind2(DARR)
  RBRACE
  { B.natelim ~scrut ~motive ~case_zero ~case_suc }
| LPAREN left = term COMMA right = term RPAREN
  { B.pair ~left ~right }
| LPAREN tm = term COLON ty = ty RPAREN
  { B.annot ~tm ~ty }

%inline term:
| located(term_) { $1 }

%inline ty: term { $1 }

bind1(SEP):
| p = pattern SEP t = term { Build.bound1 p t }

bind2(SEP):
| p1 = pattern COMMA p2 = pattern SEP t = term { Build.bound2 p1 p2 t }

pattern_:
| UNDERSCORE { B.pwildcard }
| name = name { B.pvar ~name }

%inline pattern:
| located(pattern_) { $1 }

hyp:
| p = pattern COLON ty = ty { (p, ty) }

annot:
| COLON ty = ty { ty }

phrase_desc:
| VAL name = name ty = option(annot) EQ def = term
  { B.val_ ~name ?ty ~def }
| EVAL def = term
  { B.eval ~def }

%inline phrase:
| p = located(phrase_desc) { p }

file:
| xs = phrase* EOF { xs }
| error { Error.syntax "syntax error" $startpos $endpos }

%%
