open Sexplib.Std

type idx = int [@@deriving sexp_of]

type term_desc =
  | Var of idx
  | Lam of bound1
  | App of term * term
  | Forall of term * bound1
  | Let of term * term * bound1
  | Type
  | Nat
  | Zero
  | Succ of term
  | Natelim of { discr : term;
                 motive : bound1;
                 case_zero : term;
                 case_succ : bound1; }

and term =
  {
    t_desc : term_desc;
    t_loc : Position.t;
  }

and bound1 =
  Bound1 of {
      body : term;
      user : Raw.name option;
    }

and phrase_desc =
  | Val of Raw.name * term * term

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.t;
  }

and t = phrase list [@@deriving sexp_of]

type ty = term

module Env = struct
  type t = E of Raw.name list * int

  let initial = E ([], 0)

  let lookup : t -> int -> Raw.name =
    fun (E (names, _)) i ->
    try List.nth names i
    with Not_found -> "_x" ^ string_of_int @@ (i - List.length names)

  let extend : t -> Raw.name option -> Raw.name * t =
    fun (E (names, cpt)) u ->
    let name, cpt =
      match u with
      | Some name -> name, cpt
      | None -> "_y" ^ string_of_int cpt, cpt + 1
    in
    name, E (name :: names, cpt)
end

let rec raw_of_desc env = function
  | Var i ->
     Raw.Var (Env.lookup env i)
  | Lam b ->
     Raw.Lam (weakened_of_bound1 env b)
  | App (t, u) ->
     Raw.App (raw_of env t, raw_of env u)
  | Forall (t, u) ->
     Raw.Forall (raw_of env t, weakened_of_bound1 env u)
  | Let _ ->
     assert false               (* TODO *)
  | Type ->
     Raw.Type
  | Nat ->
     Raw.Nat
  | Zero ->
     Raw.Zero
  | Succ t ->
     Raw.Succ (raw_of env t)
  | Natelim { discr; motive; case_zero; case_succ; } ->
     Raw.Natelim { discr = raw_of env discr;
                   motive = weakened_of_bound1 env motive;
                   case_zero = raw_of env case_zero;
                   case_succ = weakened_of_bound1 env case_succ; }

and raw_of env { t_desc; t_loc; } =
  Position.{ value = raw_of_desc env t_desc; position = t_loc; }

and weakened_of_bound1 env (Bound1 { body; user; }) =
  let name, env = Env.extend env user in
  Raw.Build.pvar name, raw_of env body

let rec raw_of_phrase_desc env = function
  | Val (name, ty, body) ->
     let _, new_env = Env.extend env (Some name) in
     new_env, Raw.Val (name, raw_of env ty, raw_of new_env body)

and raw_of_phrase env { ph_desc; ph_loc; } =
  let env, ph_desc = raw_of_phrase_desc env ph_desc in
  env, Position.{ value = ph_desc; position = ph_loc; }

let raw_of_file env phs = List.fold_left_map raw_of_phrase env phs

module Build = struct
  let lam bound = { t_desc = Lam bound; t_loc = Position.dummy; }

  let zero = { t_desc = Zero; t_loc = Position.dummy; }

  let succ t = { t_desc = Succ t; t_loc = Position.dummy; }
end

module PPrint = struct
  let term te =
    Raw.PPrint.term (raw_of Env.initial te)

  let phrase ph =
    let _, raw = raw_of_phrase Env.initial ph in
    Raw.PPrint.phrase raw

  let file phs =
    let _, raw = raw_of_file Env.initial phs in
    Raw.PPrint.file raw
end
