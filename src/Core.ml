open Sexplib.Std

type term_desc =
  | Var of DeBruijn.Ix.t
  | Let of { def : term; ty : term; body : bound1; }
  | Forall of term * bound1
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

and term =
  {
    t_desc : term_desc;
    t_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and bound1 =
  Bound1 of {
      body : term;
      user : Name.t option; [@equal fun _ _ -> true]
    }

and bound2 =
  Bound2 of {
      body : term;
      user1 : Name.t option; [@equal fun _ _ -> true]
      user2 : Name.t option; [@equal fun _ _ -> true]
    }

and phrase_desc =
  | Val of { user : Name.t option; ty : term; body : term; }

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and t = phrase list [@@deriving sexp_of, eq]

type ty = term

let extend env p =
  let name =
    match p with
    | Some name -> name
    | None -> "_x" ^ string_of_int (DeBruijn.Env.width env)
  in
  name, DeBruijn.Env.extend name env

let rec raw_of_desc env = function
  | Var ix ->
     Raw.Var (DeBruijn.Env.lookup env ix)
  | Lam b ->
     Raw.Lam (bound1 env b)
  | App (t, u) ->
     Raw.App (raw_of env t, raw_of env u)
  | Forall (t, u) ->
     Raw.Forall (raw_of env t, bound1 env u)
  | Let _ ->
     assert false               (* TODO *)
  | Type ->
     Raw.Type
  | Nat ->
     Raw.Nat
  | Zero ->
     Raw.Zero
  | Suc t ->
     Raw.Suc (raw_of env t)
  | Natelim { scrut; motive; case_zero; case_succ; } ->
     Raw.Natelim { scrut = raw_of env scrut;
                   motive = bound1 env motive;
                   case_zero = raw_of env case_zero;
                   case_succ = bound2 env case_succ; }

and raw_of env { t_desc; t_loc; } =
  Position.{ value = raw_of_desc env t_desc; position = t_loc; }

and bound1 env (Bound1 { body; user; }) =
  let name, env = extend env user in
  Raw.Bound1 { pat = Raw.Build.pvar name; body = raw_of env body; }

and bound2 env (Bound2 { body; user1; user2; }) =
  let name1, env = extend env user1 in
  let name2, env = extend env user2 in
  Raw.Bound2 { pat1 = Raw.Build.pvar name1;
               pat2 = Raw.Build.pvar name2;
               body = raw_of env body; }

let rec raw_of_phrase_desc env = function
  | Val { user; ty; body; } ->
     let ty = raw_of env ty in
     let body = raw_of env body in
     let name, new_env = extend env user in
     new_env, Raw.Val { name; ty; body; }

and raw_of_phrase env { ph_desc; ph_loc; } =
  let env, ph_desc = raw_of_phrase_desc env ph_desc in
  env, Position.{ value = ph_desc; position = ph_loc; }

let raw_of_file env phs = List.fold_left_map raw_of_phrase env phs

module Build = struct
  let desc ?loc t_desc =
    { t_desc; t_loc = Option.value ~default:Position.dummy loc; }

  let var ?loc ix = desc ?loc @@ Var ix

  let let_ ?loc ~def ~ty ~body () = desc ?loc @@ Let { def; ty; body; }

  let forall ?loc a f = desc ?loc @@ Forall (a, f)

  let lam ?loc bound = desc ?loc @@ Lam bound

  let app ?loc t u = desc ?loc @@ App (t, u)

  let nat ?loc () = desc ?loc Nat

  let zero ?loc () = desc ?loc Zero

  let succ ?loc t = desc ?loc @@ Suc t

  let natelim ?loc ~scrut ~motive ~case_zero ~case_succ () =
    desc ?loc @@ Natelim { scrut; motive; case_zero; case_succ; }

  let typ ?loc () = desc ?loc Type

  let val_ ?(loc = Position.dummy) ?user ~ty ~body () =
    { ph_loc = loc; ph_desc = Val { user; ty; body; } }
end

module PPrint = struct
  let term te =
    sexp_of_term te |> Sexplib.Sexp.to_string_hum |> PPrint.string

  let phrase ph =
    sexp_of_phrase ph |> Sexplib.Sexp.to_string_hum |> PPrint.string

  let file phs =
    let _, raw = raw_of_file DeBruijn.Env.empty phs in
    Raw.PPrint.file raw
end
