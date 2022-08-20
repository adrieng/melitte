open Sexplib.Std

type term_desc =
  | Var of DeBruijn.Ix.t
  | Let of def * term
  | Pi of term * bound1
  | Lam of bound1
  | App of term * term
  | Sigma of term * bound1
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

and term =
  {
    t_desc : term_desc;
    t_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and telescope = hypothesis list

and hypothesis =
  H of {
      user : Name.t option; [@equal fun _ _ -> true]
      ty : term;
      loc : Position.t;
      [@equal fun _ _ -> true]
      [@sexp_drop_if fun _ -> true]
    }

and boundN =
  BoundN of {
      tele : telescope;
      body : term;
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

and def =
  Def of {
      user : Name.t option; [@equal fun _ _ -> true]
      body : annotated;
    }

and annotated =
  Ann of {
      body : term;
      ty : term;
    }

and phrase_desc =
  | Val of def
  | Eval of { def : term; ty : term; }

and phrase =
  {
    ph_desc : phrase_desc;
    ph_loc : Position.t;
    [@equal fun _ _ -> true]
    [@sexp_drop_if fun _ -> true]
  }

and t = phrase list [@@deriving sexp_of, eq]

type ty = term

module ToRaw = struct
  type env = Name.t DeBruijn.Env.t

  module M = Monad.Reader(struct type t = env end)
  open Monad.Notation(M)

  let lookup ix env = DeBruijn.Env.lookup env ix

  let (let$) p f env =
    let name =
      match p with
      | Some name -> name
      | None -> "_x" ^ string_of_int (DeBruijn.Env.width env)
    in
    f (Raw.Build.pvar ~name ()) (DeBruijn.Env.extend name env)

  let rec term { t_desc; t_loc; } =
    let* desc =
      match t_desc with
      | Var ix ->
         let* x = DeBruijn.Env.lookup ix in
         return @@ Raw.Var x
      | Lam b ->
         let* b = bound1 b in
         return @@ Raw.Lam b
      | App (t, u) ->
         let* t = term t in
         let* u = term u in
         return @@ Raw.App (t, u)
      | Pi (a, f) ->
         let* a = term a in
         let* f = bound1 f in
         return @@ Raw.Pi (a, f)
      | Sigma (a, f) ->
         let* a = term a in
         let* f = bound1 f in
         return @@ Raw.Sigma (a, f)
      | Pair (l, r) ->
         let* l = term l in
         let* r = term r in
         return @@ Raw.Pair (l, r)
      | Fst m ->
         let* m = term m in
         return @@ Raw.Fst m
      | Snd m ->
         let* m = term m in
         return @@ Raw.Snd m
      | Let (d, body) ->
         let* d, body = def d (term body) in
         return @@ Raw.Let (d, body)
      | Type l ->
         return @@ Raw.Type l
      | Nat ->
         return Raw.Nat
      | Zero ->
         return Raw.Zero
      | Suc t ->
         let* t = term t in
         return @@ Raw.Suc t
      | Natelim { scrut; motive; case_zero; case_suc; } ->
         let* scrut = term scrut in
         let* motive = bound1 motive in
         let* case_zero = term case_zero in
         let* case_suc = bound2 case_suc in
         return @@ Raw.Natelim { scrut; motive; case_zero; case_suc; }
    in
    return @@ Position.with_pos t_loc desc

  and bound1 (Bound1 { body; user; }) =
    let$ pat = user in
    let* body = term body in
    return @@ Raw.Bound1 { pat; body; }

  and bound2 (Bound2 { body; user1; user2; }) =
    let$ pat1 = user1 in
    let$ pat2 = user2 in
    let* body = term body in
    return @@ Raw.Bound2 { pat1; pat2; body; }

  and boundN (BoundN { tele; body; }) =
    let* tele, body = telescope tele (term body) in
    return @@ Raw.BoundN { tele; body; }

  and telescope : 'a. telescope -> 'a M.t -> (Raw.telescope * 'a) M.t =
    fun tele k ->
    match tele with
    | [] ->
       let* final = k in
       return ([], final)
    | H { user; ty; loc; } :: tele ->
       let* ty = term ty in
       let$ bound = user in
       let* tele, res = telescope tele k in
       return @@ (Position.with_pos loc (Raw.H { bound; ty; }) :: tele, res)

  and annot (Ann { ty; body; }) =
    let* ty = term ty in
    let* body = term body in
    return @@ Raw.Ann { ty; body; }

  and def : 'a. def -> 'a M.t -> (Raw.def * 'a) M.t =
    fun (Def { user; body; }) k ->
    let* body = annot body in
    let$ pat = user in
    let* res = k in
    return (Raw.Def { pat; args = []; body; }, res)

  let phrase { ph_desc; ph_loc; } file =
    let* desc, file =
      match ph_desc with
      | Val d ->
         let* def, file = def d file in
         return (Raw.Val def, file)
      | Eval { ty; def; } ->
         let* ty = term ty in
         let* def = term def in
         let* file = file in
         return (Raw.Eval { ty; def; }, file)
    in
    return @@ Position.with_pos ph_loc desc :: file

  let file file =
    List.fold_right phrase file (return [])
end

module Build = struct
  type 'a locator = ?loc:Position.t -> 'a

  let desc ?loc t_desc =
    { t_desc; t_loc = Option.value ~default:Position.dummy loc; }

  let var ?loc ix = desc ?loc @@ Var ix

  let let_ ?loc def body = desc ?loc @@ Let (def, body)

  let pi ?loc a f = desc ?loc @@ Pi (a, f)

  let lam ?loc bound = desc ?loc @@ Lam bound

  let app ?loc t u = desc ?loc @@ App (t, u)

  let sigma ?loc a f = desc ?loc @@ Sigma (a, f)

  let pair ?loc left right = desc ?loc @@ Pair (left, right)

  let fst ?loc arg = desc ?loc @@ Fst arg

  let snd ?loc arg = desc ?loc @@ Snd arg

  let nat ?loc () = desc ?loc Nat

  let zero ?loc () = desc ?loc Zero

  let suc ?loc t = desc ?loc @@ Suc t

  let natelim ?loc ~scrut ~motive ~case_zero ~case_suc () =
    desc ?loc @@ Natelim { scrut; motive; case_zero; case_suc; }

  let typ ?loc level = desc ?loc @@ Type level

  let val_ ?(loc = Position.dummy) def =
    { ph_loc = loc; ph_desc = Val def; }

  let eval ?(loc = Position.dummy) ty def =
    { ph_loc = loc; ph_desc = Eval { def; ty; } }
end

module PPrint = struct
  let file file = Raw.PPrint.file (ToRaw.file file DeBruijn.Env.empty)
end
