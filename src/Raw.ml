type name = string [@@deriving show]

type pattern_desc =
  | PWildcard
  | PVar of name

and pattern = pattern_desc Position.located

and 'a weakened = pattern * 'a

and term_desc =
  | Var of name
  | Lam of term weakened
  | App of term * term
  | Forall of ty * ty weakened
  | Let of { bound : term;
             ty : ty;
             body : term weakened; }
  | Type
  | Nat | Zero | Succ
  | Natelim of { discr : term;
                 motive : term weakened option;
                 case_zero : term;
                 case_succ : term weakened; }

and term = term_desc Position.located

and ty = term

and phrase_desc =
  | Val of name * ty * term

and phrase = phrase_desc Position.located

and t = phrase list [@@deriving show]

module Build = struct
  let pvar id =
    Position.unknown_pos (PVar id)

  let var id =
    Position.unknown_pos (Var id)

  let succ t =
    Position.{ value = App (unknown_pos Succ, t);
               position = t.position; }

  let lambda ids body =
    if ids = [] then invalid_arg "lambda: empty argument list";
    List.fold_right
      (fun id body ->
        Position.{ value = Lam (id, body);
                   position = join id.position body.position; })
      ids
      body

  let forall cx b =
    List.fold_right
      (fun (p, a) b -> Position.{ value = Forall (a, (p, b));
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

  let name = string

  let pattern_desc = function
    | PWildcard -> string "_"
    | PVar x -> name x

  let pattern = Position.located pattern_desc

  let rec term_desc = function
    | Var x ->
       name x
    | Lam w ->
       binder U.(doc lambda) U.(doc darrow) w
    | Type ->
       U.(doc typ)
    | App (t, u) ->
       let rec print_app u = match u.Position.value with
         | App (u1, u2) -> simple_term u1 ^/^ print_app u2
         | u -> simple_term_desc u
       in
       simple_term t ^/^ print_app u
    | Nat ->
       U.(doc nat)
    | Zero ->
       string "zero"
    | Succ ->
       string "succ"

    | Forall (_, ({ Position.value = PVar _; _ }, _)) as t ->
       let rec print_forall = function
         | Forall (a, ({ Position.value = PVar _; _ } as p, c)) ->
            parens (hyp p a) ^/^ print_forall c.Position.value
         | t ->
            U.(doc sarrow) ^/^ term_desc t
       in
       group (U.(doc forall) ^/^ print_forall t)

    | Forall (_, ({ Position.value = PWildcard; _ }, _)) as t ->
       let rec print_fun = function
         | Forall (a, ({ Position.value = PWildcard; _ }, c)) ->
            typ a ^/^ print_fun c.Position.value
         | t ->
            U.(doc sarrow) ^/^ term_desc t
       in
       group (U.(doc forall) ^/^ print_fun t)

    | Let { bound; ty; body = (p, body); } ->
       group
         (
           (group (!^ "let" ^/^ hyp p ty
                   ^/^ !^ " =" ^/^ term bound ^/^ !^ "in"))
           ^/^ term body
         )
    | Natelim { discr; motive; case_zero; case_succ; } ->
       let m = match motive with
         | None -> empty
         | Some m -> binder (!^ " with") U.(doc darrow) m
       in
       prefix 2 1
         (group (string "elim" ^/^ term discr ^^ m))
         (braces (def empty U.(doc darrow) (!^ "zero") (term case_zero)
                  ^/^ (binder bar U.(doc darrow) case_succ)))

  and simple_term_desc = function
    | (Var _ | Type | Nat | Zero | Succ | Forall _) as t ->
       term_desc t
    | t ->
       parens (term_desc t)

  and term t : PPrint.document = Position.located term_desc t

  and simple_term t = Position.located simple_term_desc t

  and typ ty = term ty

  and def kw sep h body =
    prefix 2 1 (group (kw ^^ space ^^ h ^^ space ^^ sep)) body

  and binder kw sep (p, t) =
    def kw sep (pattern p) (term t)

  and hyp (p : pattern) ty =
    group (pattern p ^^ space ^^ colon ^/^ typ ty)

  and phrase_desc = function
    | Val (x, ty, t) ->
       def (string "val") equals (hyp (Build.pvar x) ty) (term t)

  and phrase p = Position.located phrase_desc p

  and file phrs = separate_map hardline phrase phrs
end
