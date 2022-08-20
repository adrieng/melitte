type pattern_desc =
  | PWildcard
  | PVar of Name.t

and pattern = pattern_desc Position.located

type term_desc =
  | Var of Name.t
  | Let of def * term
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

and telescope = hypothesis list

and hypothesis_desc =
  H of {
      bound : pattern;
      ty : term;
    }

and hypothesis = hypothesis_desc Position.located

and boundN =
  BoundN of {
      tele : telescope;
      body : term;
    }

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

and def =
  Def of {
      pat : pattern;
      args : telescope;
      body : annotated;
    }

and annotated =
  Ann of {
      body : term;
      ty : ty;
    }

type phrase_desc =
  | Val of def
  | Eval of { def : term; ty : ty; }

and phrase = phrase_desc Position.located

type t = phrase list

module Build = struct
  type 'a builder = ?loc:Position.t -> unit -> 'a

  let pvar ~name ?(loc = Position.dummy) () =
    Position.with_pos loc @@ PVar name

  let pwildcard ?(loc = Position.dummy) () =
    Position.with_pos loc @@ PWildcard

  let bound1 pat body =
    Bound1 { pat; body; }

  let bound2 pat1 pat2 body =
    Bound2 { pat1; pat2; body; }

  let var ~name ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Var name

  let let_ ~def ~body ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Let (def, body)

  let binder_n ~binder ~params ~body ?(loc = Position.dummy) () =
    Position.{
        (List.fold_right
           (fun (p, a) b ->
             with_pos (join (join p.position a.position) b.position)
               (binder a (bound1 p b))) params body)
      with position = loc;
    }

  let pi ~dom ~cod ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Pi (dom, cod)

  let pi_n =
    binder_n ~binder:(fun dom cod -> Pi (dom, cod))

  let arrow ~dom ~cod ?(loc = Position.dummy) () =
    pi ~loc ~dom ~cod:(bound1 (pwildcard ~loc ()) cod) ()

  let lam ~param ~body ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Lam (bound1 param body)

  let lam_n ~params ~body ?(loc = Position.dummy) () =
    List.fold_right (fun param body -> lam ~loc ~param ~body ()) params body

  let app ~func ~arg ?(loc = Position.dummy) () =
    Position.with_pos loc @@ App (func, arg)

  let app_n ~func ~args ?(loc = Position.dummy) () =
    List.fold_left (fun func arg -> app ~loc ~func ~arg ()) func args

  let sigma ~base ~total ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Sigma (base, total)

  let sigma_n =
    binder_n ~binder:(fun dom cod -> Sigma (dom, cod))

  let product ~left ~right ?(loc = Position.dummy) () =
    sigma ~loc ~base:left ~total:(bound1 (pwildcard ~loc ()) right) ()

  let pair ~left ~right ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Pair (left, right)

  let fst ~arg ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Fst arg

  let snd ~arg ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Snd arg

  let nat ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Nat

  let zero ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Zero

  let suc ~t ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Suc t

  let lit ~k ?(loc = Position.dummy) () =
    Sigs.Int.fold (fun t -> suc ~loc ~t ()) k (zero ~loc ())

  let natelim ~scrut ~motive ~case_zero ~case_suc ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Natelim { scrut; motive; case_zero; case_suc; }

  let typ ~level ?(loc = Position.dummy) () =
    if level < 0 then invalid_arg "typ";
    Position.with_pos loc @@ Type level

  let hypothesis ~bound ~ty ?(loc = Position.dummy) () =
    Position.with_pos loc @@ H { bound; ty; }

  let def ~pat ~args ~ty ~body ?(loc = Position.dummy) () =
    ignore loc;
    Def { pat; args; body = Ann { ty; body; }; }

  let val_ ~def ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Val def

  let eval ~ty ~def ?(loc = Position.dummy) () =
    Position.with_pos loc @@ Eval { ty; def; }
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

    | Let (d, body) ->
       group @@ def "let" d ^^ space ^^ !^ "in" ^/^ term body

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

  and def kw (Def { pat; args; body = Ann { ty; body; }; }) =
    prefix 2 1
      (!^ kw ^^ space ^^ pattern pat ^^ space ^^
         group (telescope args ^/^ colon ^^ term ty ^^ space ^^ equals))
      (term body)

  and hypothesis hyp =
    let H { bound; ty; } = Position.value hyp in
    parens @@ group @@ pattern bound ^/^ colon ^^ space ^^ term ty

  and telescope tele = separate_map (break 1) hypothesis tele

  and phrase_desc = function
    | Val d ->
       def "val" d
    | Eval { def; ty; } ->
       bindN (!^ "eval") colon [term def] (term ty)

  and phrase p = Position.located phrase_desc p

  and file phrs = separate_map hardline phrase phrs
end

let name_option_of_pattern Position.{ value; _ } =
  match value with
  | PWildcard -> None
  | PVar x -> Some x

let name_of_pattern p =
  Name.of_option @@ name_option_of_pattern p

