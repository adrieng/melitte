type pattern_desc =
  | PWildcard
  | PVar of Name.t

and pattern = pattern_desc Position.located

type term_desc =
  | Var of Name.t
  | Let of { def : term; ty : ty; body : bound1; }
  | Pi of ty * bound1
  | Lam of bound1
  | App of term * term
  | Sigma of ty * bound1
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Nat
  | Zero
  | Suc of term
  | Natelim of { scrut : term;
                 motive : bound1;
                 case_zero : term;
                 case_suc : bound2; }
  | Type of int

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
  let pvar ?(loc = Position.dummy) ~name () =
    Position.with_pos loc @@ PVar name

  let pwildcard ?(loc = Position.dummy) () =
    Position.with_pos loc @@ PWildcard

  let bound1 pat body =
    Bound1 { pat; body; }

  let bound2 pat1 pat2 body =
    Bound2 { pat1; pat2; body; }

  let var ?(loc = Position.dummy) ~name () =
    Position.with_pos loc @@ Var name

  let let_ ?(loc = Position.dummy) ~def ~ty ~body () =
    Position.with_pos loc @@ Let { def; ty; body; }

  let binder_n ?(loc = Position.dummy) ~binder ~params ~body () =
    Position.{
        (List.fold_right
           (fun (p, a) b ->
             with_pos (join (join p.position a.position) b.position)
               (binder a (bound1 p b))) params body)
      with position = loc;
    }

  let pi ?(loc = Position.dummy) ~dom ~cod () =
    Position.with_pos loc @@ Pi (dom, cod)

  let pi_n =
    binder_n ~binder:(fun dom cod -> Pi (dom, cod))

  let arrow ?(loc = Position.dummy) ~dom ~cod () =
    pi ~loc ~dom ~cod:(bound1 (pwildcard ~loc ()) cod) ()

  let lam ?(loc = Position.dummy) ~param ~body () =
    Position.with_pos loc @@ Lam (bound1 param body)

  let lam_n ?(loc = Position.dummy) ~params ~body () =
    List.fold_right (fun param body -> lam ~loc ~param ~body ()) params body

  let app ?(loc = Position.dummy) ~func ~arg () =
    Position.with_pos loc @@ App (func, arg)

  let app_n ?(loc = Position.dummy) ~func ~args () =
    List.fold_left (fun func arg -> app ~loc ~func ~arg ()) func args

  let sigma ?(loc = Position.dummy) ~base ~total () =
    Position.with_pos loc @@ Sigma (base, total)

  let sigma_n =
    binder_n ~binder:(fun dom cod -> Sigma (dom, cod))

  let product ?(loc = Position.dummy) ~left ~right () =
    sigma ~loc ~base:left ~total:(bound1 (pwildcard ~loc ()) right) ()

  let pair ?(loc = Position.dummy) ~left ~right () =
    Position.with_pos loc @@ Pair (left, right)

  let fst ?(loc = Position.dummy) ~arg () =
    Position.with_pos loc @@ Fst arg

  let snd ?(loc = Position.dummy) ~arg () =
    Position.with_pos loc @@ Snd arg

  let nat ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Nat

  let zero ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Zero

  let suc ?(loc = Position.dummy) ~t () =
    Position.with_pos loc @@ Suc t

  let lit ?(loc = Position.dummy) ~k () =
    Sigs.Int.fold (fun t -> suc ~loc ~t ()) k (zero ~loc ())

  let natelim ?(loc = Position.dummy) ~scrut ~motive ~case_zero ~case_suc () =
    Position.with_pos loc @@ Natelim { scrut; motive; case_zero; case_suc; }

  let typ ?(loc = Position.dummy) ~level () =
    if level < 0 then invalid_arg "typ";
    Position.with_pos loc @@ Type level

  let val_ ?(loc = Position.dummy) ~name ~ty ~body () =
    Position.with_pos loc @@ Val { name; ty; body; }

  let eval ?(loc = Position.dummy) ~ty ~body () =
    Position.with_pos loc @@ Eval { ty; body; }
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
    | (Var _ | Type _ | Nat | Zero | Suc _ | App _ | Fst _ | Snd _) as t ->
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
         U.(doc drarrow)
         patterns
         (term_desc body)

    | Let { def; ty; body = Bound1 { pat; body; }; } ->
       group
         (
           (group (!^ "let" ^/^ hyp pat ty
                   ^/^ !^ " =" ^/^ term def ^/^ !^ "in"))
           ^/^ term body
         )

    | Pi (_, Bound1 { pat = { Position.value = PVar _; _ }; _ }) as t ->
       let rec print_forall = function
         | Pi (a, Bound1 { pat = { Position.value = PVar _; _ } as pat;
                               body; }) ->
            parens (hyp pat a) ^/^ print_forall body.Position.value
         | t ->
            U.(doc srarrow) ^/^ typ_desc t
       in
       group (U.(doc forall) ^/^ print_forall t)

    | Pi (_, Bound1 { pat = { Position.value = PWildcard; _ }; _ }) as t ->
       let rec print_fun = function
         | Pi (a, Bound1 { pat = { Position.value = PWildcard; _ };
                               body; }) ->
            typ a ^^ space ^^ U.(doc srarrow) ^/^ print_fun body.Position.value
         | t ->
            typ_desc t
       in
       group (print_fun t)

    | Sigma (_, Bound1 { pat = { Position.value = PVar _; _ }; _ }) as t ->
       let rec print_forall = function
         | Sigma (a, Bound1 { pat = { Position.value = PVar _; _ } as pat;
                               body; }) ->
            parens (hyp pat a) ^/^ print_forall body.Position.value
         | t ->
            dot ^/^ typ_desc t
       in
       group (U.(doc sigma) ^/^ print_forall t)

    | Sigma (_, Bound1 { pat = { Position.value = PWildcard; _ }; _ }) as t ->
       let rec print_prod = function
         | Sigma (a, Bound1 { pat = { Position.value = PWildcard; _ };
                              body; }) ->
            typ a ^^ space ^^ U.(doc times) ^/^ print_prod body.Position.value
         | t ->
            typ_desc t
       in
       group (print_prod t)

    | Natelim { scrut; motive; case_zero; case_suc; } ->
       let m = bind1 (!^ " with") U.(doc drarrow) motive in
       prefix 2 1
         (group (!^ "elim" ^/^ term scrut ^^ m))
         (braces @@ separate (break 1 ^^ bar)
                      [
                        bind0 (!^ " zero") U.(doc drarrow) case_zero;
                        bind2 (!^ " suc") U.(doc drarrow) case_suc;
                      ] ^^ break 1)

    | Pair (left, right) ->
       parens @@ group @@ term left ^^ comma ^/^ term right

  and simple_term_desc = function
    | (Var _ | Type _ | Nat | Zero | Suc _ | Fst _ | Snd _) as t ->
       very_simple_term_desc t

    | App (t, u) ->
       simple_term t ^/^ very_simple_term u

    | _ ->
       assert false

  and very_simple_term_desc = function
    | Var x ->
       name x

    | Type l ->
       U.(doc typ ^^ space ^^ if l = max_int then !^ "top" else ExtPrint.int l)

    | Nat ->
       U.(doc nat)

    | Zero ->
       !^ "0"

    | Suc t ->
       let rec loop = function
         | Zero ->
            1, None
         | Suc t ->
            let k, r = loop t.Position.value in
            k + 1, r
         | r ->
            1, Some r
       in
       let k, r = loop t.Position.value in
       begin match r with
       | None ->
          !^ (string_of_int k)
       | Some t ->
          Sigs.Int.fold (prefix 2 1 (!^ "suc")) k (simple_term_desc t)
       end

    | Fst t ->
       prefix 2 1 (!^ "fst") (very_simple_term t)

    | Snd t ->
       prefix 2 1 (!^ "snd") (very_simple_term t)

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

  and bound1 (Bound1 { pat; body; }) =
    pattern pat ^^ dot ^^ term body

  and bound2 (Bound2 { pat1; pat2; body; }) =
    parens (group (pattern pat1 ^^ comma ^/^ pattern pat2)) ^^ dot ^^ term body

  and hyp (p : pattern) ty =
    group (pattern p ^^ space ^^ colon ^/^ typ ty)

  and phrase_desc = function
    | Val { name; ty; body; } ->
       bindN (!^ "val") equals [hyp (Build.pvar ~name ()) ty] (term body)
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

