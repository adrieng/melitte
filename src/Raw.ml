type pattern_desc =
  | PWildcard
  | PVar of Name.t

and pattern = pattern_desc Position.located

type term_desc =
  | Var of Name.t
  | Let of { def : term; ty : ty; body : bound1; }
  | Forall of ty * bound1
  | Lam of bound1
  | App of term * term
  | Nat
  | Zero
  | Suc of term
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_succ : bound2; }
  | Type

and term = term_desc Position.located

and bound1 =
  Bound1 of {
      pat : pattern;
      body : term;
    }

and bound2 =
  Bound2 of {
      pat1 : pattern;
      pat2 : pattern;
      body : term;
    }

and ty = term

type phrase_desc =
  | Val of { name : Name.t; ty : ty; body : term; }
  | Eval of { body : term; ty : ty; }

and phrase = phrase_desc Position.located

type t = phrase list

module Build = struct
  let pvar id =
    Position.unknown_pos (PVar id)

  let bound1 pat body =
    Bound1 { pat; body; }

  let bound2 pat1 pat2 body =
    Bound2 { pat1; pat2; body; }

  let var id =
    Position.unknown_pos (Var id)

  let succ t =
    Position.{ value = Suc t;
               position = t.position; }

  let lambda ids body =
    if ids = [] then invalid_arg "lambda: empty argument list";
    List.fold_right
      (fun id body ->
        Position.{ value = Lam (bound1 id body);
                   position = join id.position body.position; })
      ids
      body

  let forall cx b =
    List.fold_right
      (fun (p, a) b -> Position.{ value = Forall (a, bound1 p b);
                                  position = join (join p.position a.position)
                                               b.position; })
      cx b

  let arrow a b =
    let p = Position.{ value = PWildcard; position = a.position; } in
    forall [(p, a)] b
end

module PPrint = struct
  open PPrint
  module U = UnicodeSigil

  let name = utf8string

  let pattern_desc = function
    | PWildcard -> !^ "_"
    | PVar x -> name x

  let pattern = Position.located pattern_desc

  let rec term_desc = function
    | (Var _ | Type | Nat | Zero | Suc _ | App _) as t ->
       group (simple_term_desc t)

    | Lam _ as t ->
       let rec lam = function
         | Lam (Bound1 { pat; body; }) ->
            let pats, body = lam body.Position.value in
            pattern pat :: pats, body
         | body ->
            [], body
       in
       let patterns, body = lam t in
       bindN
         U.(doc lambda)
         U.(doc darrow)
         patterns
         (term_desc body)

    | Let { def; ty; body = Bound1 { pat; body; }; } ->
       group
         (
           (group (!^ "let" ^/^ hyp pat ty
                   ^/^ !^ " =" ^/^ term def ^/^ !^ "in"))
           ^/^ term body
         )

    | Forall (_, Bound1 { pat = { Position.value = PVar _; _ }; _ }) as t ->
       let rec print_forall = function
         | Forall (a, Bound1 { pat = { Position.value = PVar _; _ } as pat;
                               body; }) ->
            parens (hyp pat a) ^/^ print_forall body.Position.value
         | t ->
            U.(doc sarrow) ^/^ typ_desc t
       in
       group (U.(doc forall) ^/^ print_forall t)

    | Forall (_, Bound1 { pat = { Position.value = PWildcard; _ }; _ }) as t ->
       let rec print_fun = function
         | Forall (a, Bound1 { pat = { Position.value = PWildcard; _ };
                               body; }) ->
            typ a ^^ space ^^ U.(doc sarrow) ^/^ print_fun body.Position.value
         | t ->
            typ_desc t
       in
       group (print_fun t)

    | Natelim { scrut; motive; case_zero; case_succ; } ->
       let m = bind1 (!^ " with") U.(doc darrow) motive in
       prefix 2 1
         (group (!^ "elim" ^/^ term scrut ^^ m))
         (braces @@ separate (break 1 ^^ bar)
                      [
                        bind0 (!^ " zero") U.(doc darrow) case_zero;
                        bind2 (!^ " succ") U.(doc darrow) case_succ;
                      ] ^^ break 1)

  and simple_term_desc = function
    | (Var _ | Type | Nat | Zero | Suc _) as t ->
       very_simple_term_desc t

    | App (t, u) ->
       simple_term t ^/^ very_simple_term u

    | _ ->
       assert false

  and very_simple_term_desc = function
    | Var x ->
       name x

    | Type ->
       U.(doc typ)

    | Nat ->
       U.(doc nat)

    | Zero ->
       !^ "zero"

    | Suc t ->
       prefix 2 1 (!^ "suc") (simple_term t)

    | t ->
       parens (term_desc t)

  and term t : PPrint.document = Position.located term_desc t

  and simple_term t = Position.located simple_term_desc t

  and very_simple_term t = Position.located very_simple_term_desc t

  and typ ty = Position.located typ_desc ty

  and typ_desc tyd = term_desc tyd

  and bindN kw sep (heads : document list) body =
    prefix 2 1
      (prefix 2 1 kw (group @@ separate (break 1) (heads @ [sep])))
      body

  and bind0 kw sep body =
    bindN kw sep [] (term body)

  and bind1 kw sep (Bound1 { pat; body; }) =
    bindN kw sep [pattern pat] (term body)

  and bind2 kw sep (Bound2 { pat1; pat2; body; }) =
    bindN kw sep [pattern pat1 ^^ comma; pattern pat2] (term body)

  and hyp (p : pattern) ty =
    group (pattern p ^^ space ^^ colon ^/^ typ ty)

  and phrase_desc = function
    | Val { name; ty; body; } ->
       bindN (!^ "val") equals [hyp (Build.pvar name) ty] (term body)
    | Eval { body; ty; } ->
       bindN (!^ "eval") colon [term body] (term ty)

  and phrase p = Position.located phrase_desc p

  and file phrs = separate_map hardline phrase phrs
end

let name_option_of_pattern Position.{ value; _ } =
  match value with
  | PWildcard -> None
  | PVar x -> Some x

let name_of_pattern p =
  Name.of_option @@ name_option_of_pattern p

