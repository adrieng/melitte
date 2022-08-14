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
                 case_suc : bound2; }
  | Type of int

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
  | Eval of { body : term; ty : term; }

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
      | Forall (a, f) ->
         let* a = term a in
         let* f = bound1 f in
         return @@ Raw.Forall (a, f)
      | Let { def; ty; body; } ->
         let* def = term def in
         let* ty = term ty in
         let* body = bound1 body in
         return @@ Raw.Let { def; ty; body; }
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
  ;;

  let phrase { ph_desc; ph_loc; } =
    let* desc, env =
      match ph_desc with
      | Val { user; ty; body; } ->
         let name = Name.of_option user in
         let* body = term body in
         let* ty = term ty in
         let* env = M.get in
         return (Raw.Val { name; ty; body; }, DeBruijn.Env.extend name env)
      | Eval { ty; body; } ->
         let* ty = term ty in
         let* body = term body in
         let* env = M.get in
         return (Raw.Eval { ty; body; }, env)
    in
    return @@ (Position.with_pos ph_loc desc, env)

  let file file env =
    let file, _ =
      List.fold_left
        (fun (file, env) ph -> let ph, env = phrase ph env in ph :: file, env)
        ([], env)
        file
    in
    List.rev file
end

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

  let suc ?loc t = desc ?loc @@ Suc t

  let natelim ?loc ~scrut ~motive ~case_zero ~case_suc () =
    desc ?loc @@ Natelim { scrut; motive; case_zero; case_suc; }

  let typ ?loc ~level () = desc ?loc @@ Type level

  let val_ ?(loc = Position.dummy) ?user ~ty ~body () =
    { ph_loc = loc; ph_desc = Val { user; ty; body; } }

  let eval ?(loc = Position.dummy) ~ty ~body () =
    { ph_loc = loc; ph_desc = Eval { body; ty; } }
end

module PPrint = struct
  let file file = Raw.PPrint.file (ToRaw.file file DeBruijn.Env.empty)
end
